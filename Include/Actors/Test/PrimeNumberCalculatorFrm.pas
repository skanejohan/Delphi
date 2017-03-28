unit PrimeNumberCalculatorFrm;

{
  TPrimeNumberCalculatorForm -> Controller -> Presenter -> [Calculator]
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Actor.Types, Actor.Actor, Actor.Environment, PrimeNumberCalculatorActors,
  ExtCtrls;

type
  TPrimeNumberCalculatorForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    ThreadsEdit: TLabeledEdit;
    procedure Button1Click(Sender: TObject);
  protected
    Controller: TActorRef;
  end;

var
  PrimeNumberCalculatorForm: TPrimeNumberCalculatorForm;

implementation

{$R *.dfm}

procedure TPrimeNumberCalculatorForm.Button1Click(Sender: TObject);
var
  Presenter: TActorRef;
begin
  if ThreadsEdit.Enabled then
  begin
    NumberOfActorThreads := StrToInt(ThreadsEdit.Text);
    Presenter := ActorEnvironment.ActorOf(TPresenter, '',
      procedure(A: IActor)
      begin
        (A as TPresenter).Memo := Memo1;
      end);

    Controller := ActorEnvironment.ActorOf(TController, '',
      procedure(A: IActor)
      begin
        (A as TController).Presenter := Presenter;
      end);
    ThreadsEdit.Enabled := False;
  end;
  ActorEnvironment.SendTo('', Controller, '0001-0100');
  ActorEnvironment.SendTo('', Controller, '0101-0200');
  ActorEnvironment.SendTo('', Controller, '0201-0300');
  ActorEnvironment.SendTo('', Controller, '0301-0400');
  ActorEnvironment.SendTo('', Controller, '0401-0500');
  ActorEnvironment.SendTo('', Controller, '0501-0600');
  ActorEnvironment.SendTo('', Controller, '0601-0700');
  ActorEnvironment.SendTo('', Controller, '0701-0800');
  ActorEnvironment.SendTo('', Controller, '0801-0900');
  ActorEnvironment.SendTo('', Controller, '0901-1000');
end;

end.
