unit Actor.Remote.Remote;

interface

uses
  SyncObjs, Generics.Collections,
  Actor.Types, Actor.Actor, Actor.Environment, Actor.Remote.Intf,
  Actor.Remote.Server, Actor.Remote.Client, Actor.Remote.Encoding;

type
  TActorRemote = class(TInterfacedObject, IActorRemote)
  private
    FOnPeerAdded: TPeerEvent;
    FOnPeerRemoved: TPeerEvent;
    FOnActorRegistered: TActorEvent;
    FOnActorUnregistered: TActorEvent;
  protected
    type
      TActorClassList = class {Thread-safe list of available actor classes}
      var
        Lock: TCriticalSection;
        Items: TDictionary<String, TActorClass>;
      public
        procedure Add(A: TActorClass);
        procedure Remove(A: TActorClass);
        function Get(const ClassName: String): TActorClass;
        constructor Create;
        destructor Destroy; override;
      end;
  protected
    IP: String;
    Port: Cardinal;
    Server: TActorServer;
    Environment: TActorEnvironment;
    ActorClassList: TActorClassList;
    Clients: TObjectDictionary<String,TActorClient>; // 'IP:Port' -> client
    function GetKey(const IP: String; Port: Cardinal): String;
    function GetClient(const IP: String; Port: Cardinal; out Client: TActorClient): Boolean;
    procedure RemoveClient(const IP: String; Port: Cardinal);
    function HandleText(const Line: String): String;
  protected
    function GetOnPeerAdded: TPeerEvent;
    procedure SetOnPeerAdded(Value: TPeerEvent);
    function GetOnPeerRemoved: TPeerEvent;
    procedure SetOnPeerRemoved(Value: TPeerEvent);
    function GetOnActorRegistered: TActorEvent;
    procedure SetOnActorRegistered(Value: TActorEvent);
    function GetOnActorUnregistered: TActorEvent;
    procedure SetOnActorUnregistered(Value: TActorEvent);

    procedure Activate(const IP: String; Port: Cardinal);
    procedure Deactivate();

    function Open(const PeerIP: String; PeerPort: Cardinal): Boolean;
    function Close(const PeerIP: String; PeerPort: Cardinal): Boolean;

    function Register(const PeerIP: String; PeerPort: Cardinal; ActorClass: TActorClass): Boolean;
    function UnRegister(const PeerIP: String; PeerPort: Cardinal; ActorClass: TActorClass): Boolean;

    function ActorOf(const PeerIP: String; PeerPort: Cardinal; ActorClass: TActorClass;
                     const Parent: TActorRef=''): TActorRef;

    procedure SendTo(const PeerIP: String; PeerPort: Cardinal;
                     const Sender, Receiver: TActorRef; const M: TActorMessage);

  public
    constructor Create(Environment: TActorEnvironment);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TActorRemote.TActorClassList }

procedure TActorRemote.TActorClassList.Add(A: TActorClass);
begin
  Lock.Acquire;
  try
    if not Items.ContainsKey(A.ClassName) then
      Items.Add(A.ClassName, A);
  finally
    Lock.Release;
  end;
end;

procedure TActorRemote.TActorClassList.Remove(A: TActorClass);
begin
  Lock.Acquire;
  try
    if Items.ContainsKey(A.ClassName) then
      Items.Remove(A.ClassName);
  finally
    Lock.Release;
  end;
end;

function TActorRemote.TActorClassList.Get(const ClassName: String): TActorClass;
begin
  Lock.Acquire;
  try
    if not Items.TryGetValue(ClassName, Result) then
      Result := nil;
  finally
    Lock.Release;
  end;
end;

constructor TActorRemote.TActorClassList.Create;
begin
  Lock := TCriticalSection.Create;
  Items := TDictionary<String,TActorClass>.Create;
end;

destructor TActorRemote.TActorClassList.Destroy;
begin
  Items.Free;
  Lock.Free;
  inherited;
end;

{ TActorRemote }

function TActorRemote.GetKey(const IP: String; Port: Cardinal): String;
begin
  Result := Format('%s:%d', [IP, Port]);
end;

function TActorRemote.GetClient(const IP: String; Port: Cardinal;
  out Client: TActorClient): Boolean;
begin
  Result := Clients.TryGetValue(GetKey(IP, Port), Client);
end;

procedure TActorRemote.RemoveClient(const IP: String; Port: Cardinal);
var
  Key: String;
  Client: TActorClient;
begin
  Key := GetKey(IP, Port);
  if Clients.TryGetValue(Key, Client) then
  begin
    Client.Deactivate;
    Clients.Remove(Key);
  end;
end;

function TActorRemote.HandleText(const Line: String): String;
var
  PeerIP: String;
  PeerPort: Cardinal;
  ActorRef: String;
  ActorClass: TActorClass;
  ActorClassName, Parent, Sender, Receiver, M: String;
begin
  case CommandTypeOf(Line) of
    ctOpen:
    begin
      DecodeOpen(Line, PeerIP, PeerPort);
      if Assigned(FOnPeerAdded) then
        FOnPeerAdded(PeerIP, PeerPort);
      Open(PeerIP, PeerPort);
    end;
    ctClose:
    begin
      DecodeClose(Line, PeerIP, PeerPort);
      if Assigned(FOnPeerRemoved) then
        FOnPeerRemoved(PeerIP, PeerPort);
      RemoveClient(PeerIP, PeerPort);
    end;
    ctRegister:
    begin
      DecodeRegister(Line, PeerIP, PeerPort, ActorClassName);
      if Assigned(FOnActorRegistered) then
        FOnActorRegistered(PeerIP, PeerPort, ActorClassName);
    end;
    ctUnregister:
    begin
      DecodeUnregister(Line, PeerIP, PeerPort, ActorClassName);
      if Assigned(FOnActorUnregistered) then
        FOnActorUnregistered(PeerIP, PeerPort, ActorClassName);
    end;
    ctActorOf:
    begin
      DecodeActorOf(Line, PeerIP, PeerPort, ActorClassName, Parent);
      ActorClass := ActorClassList.Get(ActorClassName);
      if Assigned(ActorClass) then
      begin
        ActorRef := Environment.ActorOf(ActorClass, Parent);
        Result := GetKey(IP, Port) + ':' + ActorRef;
      end
      else
        Result := 'Unknown actor';
    end;
    ctSendTo:
    begin
      DecodeSendTo(Line, Sender, Receiver, M);
      Environment.SendTo(Sender, Receiver, M);
    end;
  else
    Result := 'Unknown command';
  end;
end;

function TActorRemote.GetOnPeerAdded: TPeerEvent;
begin
  Result := FOnPeerAdded;
end;

procedure TActorRemote.SetOnPeerAdded(Value: TPeerEvent);
begin
  FOnPeerAdded := Value;
end;

function TActorRemote.GetOnPeerRemoved: TPeerEvent;
begin
  Result := FOnPeerRemoved;
end;

procedure TActorRemote.SetOnPeerRemoved(Value: TPeerEvent);
begin
  FOnPeerRemoved := Value;
end;

function TActorRemote.GetOnActorRegistered: TActorEvent;
begin
  Result := FOnActorRegistered;
end;

procedure TActorRemote.SetOnActorRegistered(Value: TActorEvent);
begin
  FOnActorRegistered := Value;
end;

function TActorRemote.GetOnActorUnregistered: TActorEvent;
begin
  Result := FOnActorUnregistered;
end;

procedure TActorRemote.SetOnActorUnregistered(Value: TActorEvent);
begin
  FOnActorUnregistered := Value;
end;

procedure TActorRemote.Activate(const IP: String; Port: Cardinal);
begin
  Self.IP := IP;
  Self.Port := Port;
  Server.Activate(Port);
end;

procedure TActorRemote.Deactivate;
begin
  Server.Deactivate;
end;

function TActorRemote.Open(const PeerIP: String; PeerPort: Cardinal): Boolean;
var
  Key: String;
  Client: TActorClient;
begin
  try
    Key := GetKey(PeerIP, PeerPort);
    if not Clients.TryGetValue(Key, Client) then
    begin
      Client := TActorClient.Create;
      Client.Activate(PeerIP, PeerPort);
      Clients.Add(Key, Client);
      Client.Send(EncodeOpen(IP, Port));
    end;
    Result := True;
  except
    Result := False; // Maybe remove the client again?
  end;
end;

function TActorRemote.Close(const PeerIP: String; PeerPort: Cardinal): Boolean;
var
  Key: String;
  Client: TActorClient;
begin
  try
    Key := GetKey(PeerIP, PeerPort);
    if Clients.TryGetValue(Key, Client) then
    begin
      Client.Send(EncodeClose(IP, Port));
      RemoveClient(PeerIP, PeerPort);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TActorRemote.Register(const PeerIP: String; PeerPort: Cardinal;
  ActorClass: TActorClass): Boolean;
var
  Client: TActorClient;
begin
  try
    if GetClient(PeerIP, PeerPort, Client) then
      Client.Send(EncodeRegister(IP, Port, ActorClass.ClassName));
    ActorClassList.Add(ActorClass);
    Result := True;
  except
    Result := False;
  end;
end;

function TActorRemote.UnRegister(const PeerIP: String; PeerPort: Cardinal;
  ActorClass: TActorClass): Boolean;
var
  Client: TActorClient;
begin
  try
    if GetClient(PeerIP, PeerPort, Client) then
      Client.Send(EncodeUnregister(IP, Port, ActorClass.ClassName));
    ActorClassList.Add(ActorClass);
    Result := True;
  except
    Result := False;
  end;
end;

function TActorRemote.ActorOf(const PeerIP: String; PeerPort: Cardinal; ActorClass: TActorClass;
  const Parent: TActorRef): TActorRef;
var
  Client: TActorClient;
begin
  Result := '';
  try
    if GetClient(PeerIP, PeerPort, Client) then
      Result := Client.Send(EncodeActorOf(IP, Port, ActorClass.ClassName, Parent));
  except
  end;
end;

procedure TActorRemote.SendTo(const PeerIP: String; PeerPort: Cardinal;
  const Sender, Receiver: TActorRef; const M: TActorMessage);
var
  Client: TActorClient;
  SenderWithAddress: String;
begin
  try
    if GetClient(PeerIP, PeerPort, Client) then
    begin
      if Pos(':', Sender) = 0 then
        SenderWithAddress := GetKey(IP, Port) + ':' + Sender
      else
        SenderWithAddress := Sender;
      Client.Send(EncodeSendTo(SenderWithAddress, Receiver, M));
    end;
  except
  end;
end;

constructor TActorRemote.Create(Environment: TActorEnvironment);
begin
  Self.Environment := Environment;
  Server := TActorServer.Create(HandleText);
  ActorClassList := TActorClassList.Create;
  Clients := TObjectDictionary<String,TActorClient>.Create([doOwnsValues]);
end;

destructor TActorRemote.Destroy;
begin
  Deactivate;
  ActorClassList.Free;
  Clients.Free;
  inherited;
end;

end.
