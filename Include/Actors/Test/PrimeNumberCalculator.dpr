program PrimeNumberCalculator;

uses
  Forms,
  PrimeNumberCalculatorFrm in 'PrimeNumberCalculatorFrm.pas' {PrimeNumberCalculatorForm},
  PrimeNumberCalculatorActors in 'PrimeNumberCalculatorActors.pas',
  Actor.Actor in '..\Actor.Actor.pas',
  Actor.Environment in '..\Actor.Environment.pas',
  Actor.Mailbox in '..\Actor.Mailbox.pas',
  Actor.Scheduler in '..\Actor.Scheduler.pas',
  Actor.Types in '..\Actor.Types.pas',
  Actor.Remote.Types in '..\Actor.Remote.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPrimeNumberCalculatorForm, PrimeNumberCalculatorForm);
  Application.Run;
end.
