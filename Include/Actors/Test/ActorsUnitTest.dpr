program ActorsUnitTest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  ActorsUnitTestCases in 'ActorsUnitTestCases.pas',
  Actor.Actor in '..\Actor.Actor.pas',
  Actor.Environment in '..\Actor.Environment.pas',
  Actor.Mailbox in '..\Actor.Mailbox.pas',
  Actor.Remote.Client in '..\Actor.Remote.Client.pas',
  Actor.Remote.Encoding in '..\Actor.Remote.Encoding.pas',
  Actor.Remote.Intf in '..\Actor.Remote.Intf.pas',
  Actor.Remote.Remote in '..\Actor.Remote.Remote.pas',
  Actor.Remote.Server in '..\Actor.Remote.Server.pas',
  Actor.Scheduler in '..\Actor.Scheduler.pas',
  Actor.Types in '..\Actor.Types.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

