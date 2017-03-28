unit SocketServerClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils
  {$ifdef FPC}
  , lNet, lNetComponents
  {$else}
  , Sockets
  {$endif}
  ;

type
  TLogEvent = procedure(const Msg: String) of object;
  TRequestEvent = procedure(const Request: String; var Response: String) of object;

  {** Implements the socket server, that listens for request on a socket and generates events for
   another object to handle. }
  TSocketServer = class
  private
    FOnLog: TLogEvent;
    FOnRequest: TRequestEvent;
  protected
    ServerPort: Integer;
    {$ifdef FPC}
    TCPServer: TLTCPComponent;
    procedure TCPServerReceive(aSocket: TLSocket);
    {$else}
    TCPServer: TTCPServer;
    procedure TCPServerAccept(Sender: TObject; aSocket: TCustomIpClient);
    {$endif}
    procedure DoLog(const Msg: String);
    procedure DoRequest(const Request: String; var Response: String);
  public

    {** Generated when the socket server wants to log something. }
    property OnLog: TLogEvent read FOnLog write FOnLog;

    {** Generated when an HTTP request is received. The using class is responsible for parsing the
     request and provide a valid response. }
    property OnRequest: TRequestEvent read FOnRequest write FOnRequest;

    {** Creates the socket server, which will immediately start listening on the specified port. }
    constructor Create(aServerPort: Integer);
    destructor Destroy; override;
  end;

implementation

procedure TSocketServer.DoLog(const Msg: String);
begin
  if Assigned(OnLog) then
    OnLog(Msg);
end;

procedure TSocketServer.DoRequest(const Request: String; var Response: String);
begin
  if Assigned(OnRequest) then
    OnRequest(Request, Response);
end;

{$ifdef FPC}
procedure TSocketServer.TCPServerReceive(aSocket: TLSocket);
var
  Request, Response: String;

  procedure SendResponse;
  var
    Len: Integer;
  begin
    try
      Len := TCPServer.SendMessage(Response, aSocket);
      if Len < Length(Response) then
        DoLog('SendResponse error: ' + IntToStr(Len));
    except
      on E: Exception do
        DoLog(Format('Exception %s with message "%s" in SendResponse', [E.ClassName, E.Message]));
    end;
  end{procedure};

begin
  Response := '';
  if aSocket.GetMessage(Request) > 0 then
  begin
    DoLog('Request: ' + StringReplace(Trim(Request), #13#10, ' ', [rfReplaceAll]));
    DoRequest(Request, Response);
    DoLog('Response: ' + Response);
    SendResponse;
    TCPServer.Listen(ServerPort);
  end;
end;
{$else}
procedure TSocketServer.TCPServerAccept(Sender: TObject; aSocket: TCustomIpClient);
var
  Request, Response: String;

  procedure SendResponse;
  var
    Len: Integer;
  begin
    (* todo
    try
      Len := TCPServer.SendMessage(Response, aSocket);
      if Len < Length(Response) then
        DoLog('SendResponse error: ' + IntToStr(Len));
    except
      on E: Exception do
        DoLog(Format('Exception %s with message "%s" in SendResponse', [E.ClassName, E.Message]));
    end;*)
  end{procedure};

begin
  Response := '';
  while aSocket.Connected and (Request <> '') do
  begin
    Request := aSocket.Receiveln();
    DoLog('Request: ' + StringReplace(Trim(Request), #13#10, ' ', [rfReplaceAll]));
    DoRequest(Request, Response);
    DoLog('Response: ' + Response);
    SendResponse;
  end;
  aSocket.Close;
end;
{$endif}

constructor TSocketServer.Create(aServerPort: Integer);
begin
  ServerPort := aServerPort;
  {$ifdef FPC}
  TCPServer := TLTCPComponent.Create(nil);
  TCPServer.OnReceive := @TCPServerReceive;
  TCPServer.Listen(ServerPort);
  {$else}
  TCPServer := TTCPServer.Create(nil);
  TCPServer.OnAccept := TCPServerAccept;
  TCPServer.LocalHost := 'localhost';
  TCPServer.LocalPort := IntToStr(ServerPort);
  TCPServer.Open;
  {$endif}
end{procedure};

destructor TSocketServer.Destroy;
begin
  {$ifdef FPC}
  TCPServer.Free;
  {$else}
  TCPServer.Close;
  TCPServer.Free;
  {$endif}
  inherited;
end;

end.

