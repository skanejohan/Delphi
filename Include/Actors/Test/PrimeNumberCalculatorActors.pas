unit PrimeNumberCalculatorActors;

interface

uses
  Windows, StdCtrls, Generics.Collections, Actor.Types, Actor.Actor, Actor.Scheduler;

type

  { This actor adds the received string to the memo. }
  TPresenter = class(TActor)
  private
    FMemo: TMemo;
  protected
    StartTime: TDateTime;
    StartTick: Cardinal;
  public
    property Memo: TMemo read FMemo write FMemo;
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;

  { This actor determines, for a range of numbers, which ones are prime. For each determined prime,
    it sends a message to the controller actor. }
  TCalculator = class(TActor)
  protected
    function IsPrime(N: Cardinal): Boolean;
  public
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;

  TController = class(TActor)
  private
    FPresenter: TActorRef;
  protected
    Calculators: TList<TActorRef>;
  public
    property Presenter: TActorRef read FPresenter write FPresenter;
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Actor.Environment;

{ TPresenter }

procedure TPresenter.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  S.Block(
    procedure
    begin
      if M = 'START' then
      begin
        Memo.Lines.BeginUpdate;
        StartTick := GetTickCount;
        StartTime := Now;
      end
      else if M = 'STOP' then
      begin
        Memo.Lines.Add(DateTimeToStr(StartTime));
        Memo.Lines.Add(DateTimeToStr(Now));
        Memo.Lines.Add(IntToStr(GetTickCount - StartTick) + ' ms');
        Memo.Lines.EndUpdate;
      end
      else
        Memo.Lines.Add(M);
    end
  );
end;

{ TCCalculator }

function TCalculator.IsPrime(N: Cardinal): Boolean;
var
  i: cardinal;
  en: extended;
begin
  if N <> 2 then
  begin
    en := n;
    for i := 2 to round(sqrt(en)) do
      if n mod i = 0 then
        Exit(False);
  end;
  Result := n > 0;
end;

procedure TCalculator.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);

  procedure SleepHeavily(ms: Cardinal);
  var
    t: Cardinal;
  begin
    t := GetTickCount;
    while GetTickCount < t + ms do
      ;
  end;

var
  i, Low, High: Integer;
begin
  Low := StrToInt(Copy(M, 1, Pos('-', M)-1));
  High := StrToInt(Copy(M, Pos('-', M) + 1, MAXINT));
  for i := Low to High do
  begin
    if S.Killed then
      Exit;
    SleepHeavily(10);
    if IsPrime(i) then
      ActorEnvironment.SendTo(Ref, Parent, IntToStr(i));
  end;
  ActorEnvironment.SendTo(Ref, Parent, 'STOP');
end;

{ TController }

procedure TController.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
var
  Calculator: TActorRef;
begin
  // "XXX-YYY" from main form. Start a new calculator to calculate in given interval.
  // "START" from calculator. Increase "pending". If first calculator, tell presenter to start
  // "STOP" from calculator. Decrease "pending". If last calculator, tell presenter to stop
  // "XXX" from calculator - just transfer to presenter.
  if Pos('-', M) > 0 then
  begin
    Calculator := ActorEnvironment.ActorOf(TCalculator, Ref);
    ActorEnvironment.SendTo(Ref, Calculator, M);
    Calculators.Add(Calculator);
    if Calculators.Count = 1 then
      ActorEnvironment.SendTo(Ref, Presenter, 'START');
  end
  else if M = 'STOP' then
  begin
    Calculators.Remove(Sender);
    if Calculators.Count = 0 then
      ActorEnvironment.SendTo(Ref, Presenter, M);
  end
  else
    ActorEnvironment.SendTo(Ref, Presenter, M);
end;

constructor TController.Create;
begin
  Calculators := TList<TActorRef>.Create;
end;

destructor TController.Destroy;
begin
  Calculators.Free;
  inherited;
end;

end.
