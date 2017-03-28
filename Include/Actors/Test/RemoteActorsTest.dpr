program RemoteActorsTest;

uses
  Forms,
  RemoteActorsTest_CentralFrm in 'RemoteActorsTest_CentralFrm.pas' {RemoteActorsTest_CentralForm},
  Actor.Actor in '..\Actor.Actor.pas',
  Actor.Environment in '..\Actor.Environment.pas',
  Actor.Mailbox in '..\Actor.Mailbox.pas',
  Actor.Remote.Client in '..\Actor.Remote.Client.pas',
  Actor.Remote.Server in '..\Actor.Remote.Server.pas',
  Actor.Scheduler in '..\Actor.Scheduler.pas',
  Actor.Types in '..\Actor.Types.pas',
  RemoteActorsTest_WorkerFrm in 'RemoteActorsTest_WorkerFrm.pas' {RemoteActorsTest_WorkerForm},
  RemoteActorsTest_Actors in 'RemoteActorsTest_Actors.pas',
  Actor.Remote.Intf in '..\Actor.Remote.Intf.pas',
  Actor.Remote.Encoding in '..\Actor.Remote.Encoding.pas',
  Actor.Remote.Remote in '..\Actor.Remote.Remote.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRemoteActorsTest_CentralForm, RemoteActorsTest_CentralForm);
  Application.Run;
end.
