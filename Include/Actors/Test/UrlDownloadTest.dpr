program UrlDownloadTest;

uses
  Forms,
  UrlDownloadActorFrm in 'UrlDownloadActorFrm.pas' {Form16},
  Actor.Environment in '..\Actor.Environment.pas',
  Actor.Actor in '..\Actor.Actor.pas',
  Actor.Types in '..\Actor.Types.pas',
  Actor.Mailbox in '..\Actor.Mailbox.pas',
  Actor.Scheduler in '..\Actor.Scheduler.pas',
  Actor.Remote.Types in '..\Actor.Remote.Types.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
