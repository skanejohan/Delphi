program WebQuizServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, WebQuizServerMainFrm, LResources, lnetvisual,
  WebQuizServerManagerClass, SocketServerClass, QuizGameClass, QuizManagerClass,
  SessionClasses, WebQuizSessionClass, PreloadedIniFileQuizManagerClass
  { you can add units after this };

{$IFDEF WINDOWS}{$R WebQuizServer.rc}{$ENDIF}

begin
  {$I WebQuizServer.lrs}
  Application.Initialize;
  Application.CreateForm(TWebQuizServerMainForm, WebQuizServerMainForm);
  Application.Run;
end.

