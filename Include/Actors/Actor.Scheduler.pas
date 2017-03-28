unit Actor.Scheduler;

interface

uses
  Classes, Actor.Types, Actor.Mailbox;

type
  TActorScheduler = class(TThread)
  private
    FMailbox: TActorMailBox;
    function GetKilled: Boolean;
  protected
    procedure Execute; override;
  public
    property Mailbox: TActorMailbox read FMailBox;
    property Killed: Boolean read GetKilled;
    procedure Block(Proc: TThreadProcedure);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SyncObjs, Actor.Actor;

procedure TActorScheduler.Execute;
var
  Env: PActorMessageEnvelope;
begin
  while not Terminated do
    case FMailbox.Event.WaitFor(100) of
      wrSignaled:
        case FMailbox.Dequeue(Env, 100) of
          wrSignaled:
            try
              TActor(Env^.Receiver).Receive(Env^.M, Self, Env^.Sender);
            finally
              Dispose(Env);
            end;
          wrTimeOut:
            ; { Just keep waiting }
          wrAbandoned:
            ; { What should we do here? }
        end;
      wrTimeout:
        ; { Just keep waiting }
      wrError:
        ; { What should we do here? }
      wrAbandoned:
        ; { What should we do here? }
    end;
end;

function TActorScheduler.GetKilled: Boolean;
begin
  Result := Terminated;
end;

procedure TActorScheduler.Block(Proc: TThreadProcedure);
begin
  Synchronize(Proc);
end;

constructor TActorScheduler.Create;
begin
  FMailbox := TActorMailBox.Create;
  inherited Create(False);
end;

destructor TActorScheduler.Destroy;
begin
  FMailbox.Close;
  FMailbox.Free;
  inherited;
end;

end.
