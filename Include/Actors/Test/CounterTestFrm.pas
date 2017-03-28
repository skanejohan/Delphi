unit CounterTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Actor.Types, Actor.Actor, Actor.Scheduler, Actor.Environment, StdCtrls, ExtCtrls;

type
  TCounterTestForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  protected
    CounterActor: TActorRef;
  end;

var
  CounterTestForm: TCounterTestForm;

implementation

{$R *.dfm}

type
  TUIActor = class(TActor) {Displays the string at the given label}
  protected
    L: TLabel;
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;

  TCounterActor = class(TActor) {Modifies internal state}
  protected
    Ctr: Integer;
    UIActor: TActorRef;
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
    constructor Create; override;
  end;

procedure TUIActor.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  S.Block(
    procedure
    begin
      L.Caption := M;
    end
  );
end;

{ TCounterActor }

constructor TCounterActor.Create;
begin
  inherited;
  Ctr := 0;
end;

procedure TCounterActor.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  //Sleep(1000);
  if M = 'INC' then
    Inc(Ctr);
  if M = 'DEC' then
    Dec(Ctr);
  ActorEnvironment.SendTo(Ref, UIActor, IntToStr(Ctr));
end;

procedure TCounterTestForm.FormCreate(Sender: TObject);
var
  UIActor: TActorRef;
begin
  UIActor := ActorEnvironment.ActorOf(TUIActor, '',
    procedure (A: IActor)
    begin
      (A as TUIActor).L := Label1;
    end);

  CounterActor := ActorEnvironment.ActorOf(TCounterActor, '',
    procedure (A: IActor)
    begin
      (A as TCounterActor).UIActor := UIActor;
    end);
end;

procedure TCounterTestForm.Timer1Timer(Sender: TObject);
begin
  ActorEnvironment.SendTo('', CounterActor, 'INC');
end;

procedure TCounterTestForm.Button1Click(Sender: TObject);
begin
  ActorEnvironment.SendTo('', CounterActor, 'INC');
end;

procedure TCounterTestForm.Button2Click(Sender: TObject);
begin
  ActorEnvironment.SendTo('', CounterActor, 'DEC');
end;

procedure TCounterTestForm.CheckBox1Click(Sender: TObject);
begin
  Timer1.Enabled := CheckBox1.Checked;
end;

end.
