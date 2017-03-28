program CounterTest;

uses
  Forms,
  CounterTestFrm in 'CounterTestFrm.pas' {CounterTestForm},
  Actor.Actor in '..\Actor.Actor.pas',
  Actor.Environment in '..\Actor.Environment.pas',
  Actor.Mailbox in '..\Actor.Mailbox.pas',
  Actor.Scheduler in '..\Actor.Scheduler.pas',
  Actor.Types in '..\Actor.Types.pas',
  Actor.Remote.Types in '..\Actor.Remote.Types.pas';

{$R *.res}

begin
  {$warn symbol_platform off}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$warn symbol_platform on}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCounterTestForm, CounterTestForm);
  Application.Run;
end.
