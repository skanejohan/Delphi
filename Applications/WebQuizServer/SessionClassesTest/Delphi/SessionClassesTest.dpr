program SessionClassesTest;

uses
  TestFramework,
  SessionClasses in '..\..\..\Include\SessionClasses.pas',
  SessionClassesTestCase in '..\SessionClassesTestCase.pas',
  GuiTestRunner,
  testsessionclasses in '..\testsessionclasses.pas';

{$R *.res}

begin
  TGUITestRunner.runRegisteredTests;
end.


