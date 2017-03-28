program SessionClassesTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, SessionClassesTestCase, LResources, 
  SessionClasses;

{$IFDEF WINDOWS}{$R SessionClassesTest.rc}{$ENDIF}

begin
  {$I SessionClassesTest.lrs}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

