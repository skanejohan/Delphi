program SessionClassesVisualTest;

uses
  Forms,
  SessionClasses in '..\..\..\Include\SessionClasses.pas',
  SessionClassesVisualTestFrm in 'SessionClassesVisualTestFrm.pas' {SessionClassesVisualTestForm},
  testsessionclasses in '..\testsessionclasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSessionClassesVisualTestForm, SessionClassesVisualTestForm);
  Application.Run;
end.

