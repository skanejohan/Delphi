unit WebQuizSessionClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, SessionClasses, QuizGameClass;

type
  TWebQuizSession = class(TSession)
  private
    FGame: TQuizGame;
  public
    property Game: TQuizGame read FGame;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

constructor TWebQuizSession.Create;
begin
  FGame := TQuizGame.Create;
end;

destructor TWebQuizSession.Destroy;
begin
  FGame.Free;
  inherited;
end;

end.

