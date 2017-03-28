unit RemoteActorsTest_Actors;

interface

uses
  StdCtrls, Actor.Types, Actor.Actor, Actor.Scheduler;

type
  TPresenter = class(TActor)
  private
    FMemo: TMemo;
  public
    property Memo: TMemo read FMemo write FMemo;
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;

  TWorker = class(TActor)
  protected
    Value: Integer;
  public
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
    constructor Create(Owner: TObject); override;
  end;

implementation

uses
  SysUtils;

procedure TPresenter.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  S.Block(
    procedure
    begin
      FMemo.Lines.Add(M);
    end);
end;

procedure TWorker.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  if M = 'INC' then
    Inc(Value);
  if M = 'DEC' then
    Dec(Value);
  if M = 'ASK' then
    SendTo(Sender, IntToStr(Value))

//  if M = 'time' then
//    SendTo(Parent, DateTimeToStr(Now))
//  else
//    Stop;
end;

constructor TWorker.Create(Owner: TObject);
begin
  inherited;
  Value := 0;
end;

end.
