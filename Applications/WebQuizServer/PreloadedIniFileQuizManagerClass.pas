unit PreloadedIniFileQuizManagerClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, QuizManagerClass;

type
  TPreloadedIniFileQuizManager = class(TQuizManager)
  public
    constructor Create; override;
  end{class};

implementation

uses
  IniFiles;

constructor TPreloadedIniFileQuizManager.Create;
var
  i, j: Integer;
  IniFile: TIniFile;
  QuizNames: TStrings;
  QuizEntries: TStrings;
  QuizEntryParams: TStrings;
begin
  inherited;
  QuizNames := TStringList.Create;
  QuizEntries := TStringList.Create;
  QuizEntryParams := TStringList.Create;
  QuizEntryParams.Delimiter := '|';
  {$ifdef FPC}
  QuizEntryParams.StrictDelimiter := True;
  {$endif}
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + '..\Content\Quizes.ini');
  try
    IniFile.ReadSections(QuizNames);
    for i := 0 to QuizNames.Count-1 do
    begin
      AddQuiz(QuizNames[i]);
      IniFile.ReadSection(QuizNames[i], QuizEntries);
      for j := 0 to QuizEntries.Count-1 do
      begin
        QuizEntryParams.DelimitedText := IniFile.ReadString(QuizNames[i], QuizEntries[j], '');
        if QuizEntryParams.Count > 5 then
          AddQuestion(QuizEntryParams[0], QuizEntryParams[1], QuizEntryParams[2],
            QuizEntryParams[3], QuizEntryParams[4], StrToIntDef(QuizEntryParams[5], 0));
      end{for};
    end{for};
  finally
    QuizEntryParams.Free;
    QuizEntries.Free;
    QuizNames.Free;
    IniFile.Free;
  end;
end;

end.

