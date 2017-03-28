unit WebQuizServerManagerClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, SocketServerClass, SessionClasses, QuizManagerClass, QuizGameClass,
  WebQuizSessionClass;

type
  TRequestType = (rtLoadIndex, rtLoadjQuery, rtStartQuiz, rtAnswer);

  TWebQuizServerManager = class
  private
    FOnLog: TLogEvent;
  protected
    QuizManager: TQuizManager;
    SocketServer: TSocketServer;
    SessionManager: TSessionManager;
    procedure DoLog(const Msg: String);
    function ParseRequest(const Request: String; out QuizName: String; out SessionID: Cardinal;
      out Answer: Integer): TRequestType;
    function LoadHTML(const FileName: String): String;
    function LoadIndex: String;
    function LoadjQuery: String;
    function LoadQuestion(Game: TQuizGame): String;
    function StartQuiz(SessionID: Cardinal; Game: TQuizGame; const QuizName: String): String;
    function Answer(Game: TQuizGame; SuggestedAnswer: Integer): String;
    procedure SocketServerRequest(const Request: String; var Response: String);
  public
    property OnLog: TLogEvent read FOnLog write FOnLog;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  PreloadedIniFileQuizManagerClass;

const
  ServerPort = 9000; {Ought to be placed in external configuration}
  NumberOfQuestions = 10;

procedure TWebQuizServerManager.DoLog(const Msg: String);
begin
  if Assigned(OnLog) then
    OnLog(Msg);
end;

function TWebQuizServerManager.ParseRequest(const Request: String; out QuizName: String;
  out SessionID: Cardinal; out Answer: Integer): TRequestType;

  function ParseParameter(const Param: String): String;
  var
    s: String;
  begin
    s := Copy(Request, Pos(Param + '=', Request) + Length(Param + '='), MAXINT);
    if Pos('&', s) = 0 then
      Result := s
    else
      Result := Copy(s, 1, Pos('&', s) - 1);
    Result := StringReplace(Result, '%20', ' ', [rfReplaceAll]);
  end{function};

begin
  if Pos('jquery', Request) <> 0 then
    Result := rtLoadjQuery
  else if Pos('quizname', Request) <> 0 then
  begin
    Result := rtStartQuiz;
    QuizName := ParseParameter('quizname');
  end{if}
  else if Pos('answer', Request) <> 0 then
  begin
    Result := rtAnswer;
    SessionID := StrToIntDef(ParseParameter('sessionid'), 0);
    Answer := StrToIntDef(ParseParameter('answer'), 0);
  end{if}
  else
    Result := rtLoadIndex;
end{function};

function TWebQuizServerManager.LoadHTML(const FileName: String): String;
var
  SL: TStrings;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile('..\Content\' + FileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TWebQuizServerManager.LoadIndex: String;
var
  i: Integer;
  SL: TStrings;
  Options: String;
begin
  Options := '';
  SL := TStringList.Create;
  try
    QuizManager.GetQuizNames(SL);
    for i := 0 to SL.Count-1 do
      Options := Options + Format('<option value="%s">%s</option>', [SL[i], SL[i]]);
    Result := StringReplace(LoadHTML('index.html'), '_OPTIONS_', Options, []);
  finally
    SL.Free;
  end{finally};
end{procedure};

function TWebQuizServerManager.LoadjQuery: String;
begin
  Result := LoadHTML('jquery-1.4.4.min.js')
end{procedure};

function TWebQuizServerManager.LoadQuestion(Game: TQuizGame): String;
var
  Number: Integer;
  Q, A1, A2, A3, A4: String;
begin
  if Game.GetNextQuestion(Q, A1, A2, A3, A4, Number) then
    Result := Format('"number" : "%d", "question" : "%s", "alt1" : "%s", "alt2" : "%s", ' +
      '"alt3" : "%s", "alt4" : "%s", "done" : "no"', [Number, Q, A1, A2, A3, A4])
  else
    Result := Format('"done" : "yes", "score" : "%d/%d"', [Game.NumberOfCorrectAnswers,
      Game.NumberOfQuestions]);
end{procedure};

function TWebQuizServerManager.StartQuiz(SessionID: Cardinal; Game: TQuizGame; const QuizName: String): String;
begin
  if Game.Initialize(QuizManager, QuizName, NumberOfQuestions) then
    Result := Format('{ %s, "sessionid" : "%d" }', [LoadQuestion(Game), SessionID]);
end{procedure};

function TWebQuizServerManager.Answer(Game: TQuizGame; SuggestedAnswer: Integer): String;
var
  CorrectAnswer: Integer;
begin
  if Game.Respond(SuggestedAnswer, CorrectAnswer) then
    Result := Format('{ %s , "result" : "That was correct!" }', [LoadQuestion(Game)])
  else
    Result := Format('{ %s , "result" : "Too bad, that was incorrect!" }', [LoadQuestion(Game)]);
end{procedure};

procedure TWebQuizServerManager.SocketServerRequest(const Request: String;
  var Response: String);
var
  Game: TQuizGame;
  QuizName: String;
  SessionID: Cardinal;
  SuggestedAnswer: Integer;

  function GetGame: Boolean;
  var
    Session: TWebQuizSession;
  begin
    Session := (SessionManager.GetSession(SessionID) as TWebQuizSession);
    Result := Assigned(Session);
    if Result then
      Game := Session.Game
    else
    begin
      DoLog(Format('Incorrect session ID: %d', [SessionID]));
      Response := LoadIndex;
    end;
  end;

begin
  case ParseRequest(Request, QuizName, SessionID, SuggestedAnswer) of
    rtLoadIndex:
      Response := LoadIndex;
    rtLoadjQuery:
      Response := LoadjQuery;
    rtStartQuiz:
    begin
      SessionID := SessionManager.NewSession;
      if GetGame then
        Response := StartQuiz(SessionID, Game, QuizName);
    end;
    rtAnswer:
      if GetGame then
        Response := Answer(Game, SuggestedAnswer);
  end;
end;

constructor TWebQuizServerManager.Create;
begin
  SocketServer := TSocketServer.Create(ServerPort);
  {$ifdef FPC}
  SocketServer.OnLog := @DoLog;
  SocketServer.OnRequest := @SocketServerRequest;
  {$else}
  SocketServer.OnLog := DoLog;
  SocketServer.OnRequest := SocketServerRequest;
  {$endif}
  SessionManager := TSessionManager.Create(TWebQuizSession);
  QuizManager := TPreloadedIniFileQuizManager.Create;
end;

destructor TWebQuizServerManager.Destroy;
begin
  QuizManager.Free;
  SessionManager.Free;
  SocketServer.Free;
  inherited;
end;

end.

