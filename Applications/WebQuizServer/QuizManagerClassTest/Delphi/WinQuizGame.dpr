program WinQuizGame;

uses
  Forms,
  QuizGameClass in '..\..\..\Include\QuizGameClass.pas',
  QuizManagerClass in '..\..\..\Include\QuizManagerClass.pas',
  HardcodedQuizManagerClass in '..\HardcodedQuizManagerClass.pas',
  IniFileQuizManagerClass in '..\IniFileQuizManagerClass.pas',
  WinQuizGameFrm in 'WinQuizGameFrm.pas' {WinQuizGameForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.CreateForm(TWinQuizGameForm, WinQuizGameForm);
  Application.Run;
end.

