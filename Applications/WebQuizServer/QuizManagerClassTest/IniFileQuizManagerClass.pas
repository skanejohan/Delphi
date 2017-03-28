unit IniFileQuizManagerClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, QuizManagerClass;

type
  TIniFileQuizManager = class(TQuizManager)
  protected
    procedure InternalGetQuizNames(const QuizNames: TStrings); override;
    function InternalGetNumberOfQuestions(const QuizName: String): Integer; override;
    function InternalGetQuestion(const QuizName: String; QuestionIndex: Integer;
      out Question, Alt1, Alt2, Alt3, Alt4: String; out Correct: Integer): Boolean; override;
  end;

implementation

uses
  IniFiles;

procedure TIniFileQuizManager.InternalGetQuizNames(const QuizNames: TStrings);
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + '..\Quizes.ini') do
  begin
    ReadSections(QuizNames);
    Free;
  end;
end;

function TIniFileQuizManager.InternalGetNumberOfQuestions(const QuizName: String): Integer;
var
  Keys: TStrings;
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + '..\Quizes.ini') do
  begin
    if SectionExists(QuizName) then
    begin
      Keys := TStringList.Create;
      try
        ReadSection(QuizName, Keys);
        Result := Keys.Count;
      finally
        Keys.Free;
      end;
    end
    else
      Result := 0;
    Free;
  end;
end;

function TIniFileQuizManager.InternalGetQuestion(const QuizName: String;
  QuestionIndex: Integer; out Question, Alt1, Alt2, Alt3, Alt4: String;
  out Correct: Integer): Boolean;
var
  Params: TStrings;
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + '..\Quizes.ini') do
  begin
    if SectionExists(QuizName) then
    begin
      Params := TStringList.Create;
      try
        Params.Delimiter := '|';
        Params.StrictDelimiter := True;
        Params.DelimitedText := ReadString(QuizName, Format('Q%d', [QuestionIndex]), '');
        if Params.Count > 5 then
        begin
          Question := Params[0];
          Alt1 := Params[1];
          Alt2 := Params[2];
          Alt3 := Params[3];
          Alt4 := Params[4];
          Correct := StrToIntDef(Params[5], 0);
          Result := True;
        end
        else
          Result := False;
      finally
        Params.Free;
      end;
    end
    else
      Result := False;
    Free;
  end;
end;

end.

