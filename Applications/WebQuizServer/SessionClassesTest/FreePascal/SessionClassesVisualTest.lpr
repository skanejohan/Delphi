program SessionClassesVisualTest;

{$mode objfpc}{$H+}

uses
  heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, SessionClassesVisualTestFrm, TestSessionClasses;

{$IFDEF WINDOWS}{$R SessionClassesVisualTest.rc}{$ENDIF}

begin
  SetHeapTraceOutput(ParamStr(0) + '.heaptrace');
  {$I SessionClassesVisualTest.lrs}
  Application.Initialize;
  Application.CreateForm(TSessionClassesVisualTestForm,
    SessionClassesVisualTestForm);
  Application.Run;
end.

