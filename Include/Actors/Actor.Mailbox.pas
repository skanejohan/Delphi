unit Actor.Mailbox;

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs, Actor.Types;

type
  TActorMailBox = class
  strict private
    FEvent: TEvent;
    FQueue: TQueue<PActorMessageEnvelope>;
    FClosed: Boolean;
  public
    property Event: TEvent read FEvent;
    function Enqueue(const Item: PActorMessageEnvelope; Timeout: LongWord = INFINITE): TWaitResult;
    function Dequeue(var Item: PActorMessageEnvelope; Timeout: LongWord = INFINITE): TWaitResult;
    procedure Close;
    property Closed: Boolean read FClosed;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Diagnostics;

function TActorMailbox.Enqueue(const Item: PActorMessageEnvelope; Timeout: LongWord): TWaitResult;
begin
  if Closed then
    Exit(wrAbandoned);
  if not TMonitor.Enter(FQueue, Timeout) then
    Exit(wrTimeout);
  try
    if Closed then
      Exit(wrAbandoned);
    FQueue.Enqueue(Item);
    FEvent.SetEvent;
    TMonitor.Pulse(FQueue);
    Result := wrSignaled;
  finally
    TMonitor.Exit(FQueue);
  end;
end;

function TActorMailbox.Dequeue(var Item: PActorMessageEnvelope; Timeout: LongWord): TWaitResult;
var
  Stopwatch: TStopwatch;
  TimeoutLeft: Int64;
begin
  if Closed then
    Exit(wrAbandoned);
  Stopwatch := TStopwatch.StartNew;
  if not TMonitor.Enter(FQueue, Timeout) then
    Exit(wrTimeout);
  try
    while not Closed and (FQueue.Count = 0) do
    begin
      TimeoutLeft := Timeout - Stopwatch.ElapsedMilliseconds;
      if TimeoutLeft < 0 then
        TimeoutLeft := 0;
      if not TMonitor.Wait(FQueue, LongWord(TimeoutLeft)) then
        Exit(wrTimeout);
    end;
    if Closed then
      Exit(wrAbandoned);
    Item := FQueue.Dequeue;
    if FQueue.Count = 0 then
      Event.ResetEvent;
    Result := wrSignaled;
  finally
    TMonitor.Exit(FQueue);
  end;
end;

procedure TActorMailbox.Close;
begin
  if FClosed then
    Exit;
  FClosed := True;
  TMonitor.Enter(FQueue);
  try
    FQueue.Clear;
    TMonitor.PulseAll(FQueue); //notify any waiters Closed is now True
  finally
    TMonitor.Exit(FQueue);
  end;
end;

constructor TActorMailbox.Create;
begin
  inherited;
  FQueue := TQueue<PActorMessageEnvelope>.Create;
  FEvent := TSimpleEvent.Create;
end;

destructor TActorMailbox.Destroy;
begin
  Close;
  FQueue.Free;
  FEvent.Free;
  inherited;
end;

end.
