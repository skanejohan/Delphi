unit Actor.Environment;

interface

uses
  Classes, Generics.Collections,
  Actor.Types, Actor.Actor, Actor.Scheduler, Actor.Remote.Intf;

type
  TActorEnvironment = class
  private
    FRemote: IActorRemote;
  protected
    SchedulerIndex: Integer;
    Schedulers: Array of TActorScheduler;
    Actors: TDictionary<TActorRef,IActor>;
  public

    procedure SendTo(Sender, Receiver: TActorRef;
                     const M: TActorMessage);

    function ActorOf(ActorClass: TActorClass;
                     const Parent: TActorRef='';
                     SetupProc: TActorProc=nil): TActorRef;

    property Remote: IActorRemote read FRemote write FRemote;

    procedure RemoveActor(Ref: TActorRef);

    constructor Create(NoOfThreads: Cardinal);
    destructor Destroy; override;
  end;

var
  NumberOfActorThreads: Integer = 4;

function ActorEnvironment: TActorEnvironment;

implementation

uses
  SysUtils, SyncObjs;

var
  _ActorEnvironment: TActorEnvironment;

function ActorEnvironment: TActorEnvironment;
begin
  if not Assigned(_ActorEnvironment) then
    _ActorEnvironment := TActorEnvironment.Create(NumberOfActorThreads);
  Result := _ActorEnvironment;
end;

function TActorEnvironment.ActorOf(ActorClass: TActorClass;
  const Parent: TActorRef; SetupProc: TActorProc): TActorRef;
var
  Actor: IActor;
begin
  Inc(SchedulerIndex);
  Result := IntToStr(SchedulerIndex);
  Actor := ActorClass.Create(Self);
  (Actor as TActor).Ref := Result;
  (Actor as TActor).Parent := Parent;
  if Assigned(SetupProc) then
    SetupProc(Actor);
  Actors.Add(Result, Actor);
  (Actor as TActor).Mailbox := Schedulers[SchedulerIndex mod Length(Schedulers)].Mailbox;
end;

procedure TActorEnvironment.SendTo(Sender, Receiver: TActorRef;
  const M: TActorMessage);
var
  S: String;
  Actor: IActor;
  PeerPort: Cardinal;
  Env: PActorMessageEnvelope;
  PeerIP, PeerReceiver: String;
begin
  if (not Assigned(Remote)) or (Pos(':', Receiver) = 0) then
  begin
    if Actors.TryGetValue(Receiver, Actor) then
      try
        New(Env);
        Env^.Receiver := Actor;
        Env^.M := M;
        Env^.Sender := Sender;
        (Actor as TActor).Mailbox.Enqueue(Env, 100);
      except
      end;
  end
  else
  begin
    PeerIP := Copy(Receiver, 1, Pos(':', Receiver)-1);
    S := Copy(Receiver, Pos(':', Receiver) + 1, MAXINT);
    PeerPort := StrToIntDef(Copy(S, 1, Pos(':', S)-1), 0);
    PeerReceiver := Copy(S, Pos(':', S) + 1, MAXINT);
    Remote.SendTo(PeerIP, PeerPort, Sender, PeerReceiver, M);
  end;
end;

procedure TActorEnvironment.RemoveActor(Ref: TActorRef);
begin
  Actors.Remove(Ref);
end;

constructor TActorEnvironment.Create(NoOfThreads: Cardinal);
var
  i: Integer;
begin
  SchedulerIndex := 0;
  SetLength(Schedulers, NoOfThreads);
  for i := 0 to NoOfThreads-1 do
    Schedulers[i] := TActorScheduler.Create;
  Actors := TDictionary<TActorRef,IActor>.Create;
  FRemote := nil;
end;

destructor TActorEnvironment.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(Schedulers)-1 do
    Schedulers[i].Free;
  Actors.Free;
  FRemote := nil;
  inherited;
end;

initialization
  _ActorEnvironment := nil;

finalization
  _ActorEnvironment.Free;

end.
