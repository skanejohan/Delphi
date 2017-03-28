program WebQuizServer;

uses
  Forms, 
  WebQuizServerMainFrm in 'WebQuizServerMainFrm.pas' {WebQuizServerMainForm},
  WebQuizServerManagerClass in '..\WebQuizServerManagerClass.pas', 
  SocketServerClass in '..\SocketServerClass.pas',
  QuizGameClass in '..\QuizGameClass.pas',
  QuizManagerClass in '..\QuizManagerClass.pas',
  SessionClasses in '..\SessionClasses.pas',
  WebQuizSessionClass in '..\WebQuizSessionClass.pas', 
  PreloadedIniFileQuizManagerClass in '..\PreloadedIniFileQuizManagerClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWebQuizServerMainForm, WebQuizServerMainForm);
  Application.Run;
end.

