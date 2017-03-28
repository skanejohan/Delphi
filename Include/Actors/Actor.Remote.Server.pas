unit Actor.Remote.Server;

interface

uses
  SysUtils, IdContext, IdTCPServer;

type
  TReceivedTextHandler = reference to function(const Line: String): String;

  TActorServer = class
  protected
    TCPServer: TIdTCPServer;
    ReceivedTextHandler: TReceivedTextHandler;
    procedure TCPServerConnect(AContext: TIdContext);
    procedure TCPServerDisconnect(AContext: TIdContext);
    procedure TCPServerException(AContext: TIdContext; E: Exception);
    procedure TCPServerExecute(AContext: TIdContext);
  public
    procedure Activate(Port: Cardinal);
    procedure Deactivate;
    constructor Create(ReceivedTextHandler: TReceivedTextHandler);
  end;

implementation

uses
  Actor.Environment;

procedure TActorServer.TCPServerConnect(AContext: TIdContext);
begin
end;

procedure TActorServer.TCPServerDisconnect(AContext: TIdContext);
begin
end;

procedure TActorServer.TCPServerException(AContext: TIdContext; E: Exception);
begin
end;

procedure TActorServer.TCPServerExecute(AContext: TIdContext);
var
  Line: String;
begin
  Line := AContext.Connection.IOHandler.ReadLn;
  AContext.Connection.IOHandler.WriteLn(ReceivedTextHandler(Line));
end;

procedure TActorServer.Deactivate;
begin
  if Assigned(TCPServer) then
    TCPServer.Active := False;
  FreeAndNil(TCPServer);
end;

procedure TActorServer.Activate(Port: Cardinal);
begin
  Deactivate;
  TCPServer := TIdTCPServer.Create;
  TCPServer.Bindings.Add.IP := '127.0.0.1';
  TCPServer.Bindings.Add.Port := Port;
  TCPServer.OnConnect := TCPServerConnect;
  TCPServer.OnDisconnect := TCPServerDisconnect;
  TCPServer.OnException := TCPServerException;
  TCPServer.OnExecute := TCPServerExecute;
  TCPServer.Active := True;
end;

constructor TActorServer.Create(ReceivedTextHandler: TReceivedTextHandler);
begin
  Self.ReceivedTextHandler := ReceivedTextHandler;
end;

end.
