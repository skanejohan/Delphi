unit Actor.Remote.Client;

interface

uses
  IdTCPClient, Actor.Types;

type
  TActorClient = class
  protected
    TCPClient: TIdTCPClient;
    procedure TCPClientConnected(Sender: TObject);
    procedure TCPClientDisonnected(Sender: TObject);
  public
    procedure Activate(const IP: String; Port: Cardinal);
    function Send(const S: String): String;
    procedure Deactivate;
  end;

implementation

uses
  SysUtils;

function TActorClient.Send(const S: String): String;
begin
  TCPClient.IOHandler.WriteLn(S);
  Result := TCPClient.IOHandler.ReadLn();
end;

procedure TActorClient.TCPClientConnected(Sender: TObject);
begin
end;

procedure TActorClient.TCPClientDisonnected(Sender: TObject);
begin
end;

procedure TActorClient.Activate(const IP: String; Port: Cardinal);
begin
  Deactivate;
  TCPClient := TIdTCPClient.Create;
  TCPClient.Host := IP;
  TCPClient.Port := Port;
  TCPClient.Connect;
end;

procedure TActorClient.Deactivate;
begin
  if Assigned(TCPClient) then
    TCPClient.Disconnect;
  FreeAndNil(TCPClient);
end;

end.
