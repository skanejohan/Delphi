program WinQuizGame;

{$mode objfpc}{$H+}

uses
  heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, WinQuizGameFrm
  { you can add units after this };

{$IFDEF WINDOWS}{$R WinQuizGame.rc}{$ENDIF}

begin
  SetHeapTraceOutput(ParamStr(0) + '.heaptrace');
  {$I WinQuizGame.lrs}
  Application.Initialize;
  Application.CreateForm(TWinQuizGameForm, WinQuizGameForm);
  Application.Run;
end.

