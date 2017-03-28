unit Actor.Actor;

interface

uses
  Actor.Types, Actor.Mailbox, Actor.Scheduler;

type
  TActor = class;
  TActorClass = class of TActor;
  TActorProc = reference to procedure(A: IActor);

  TActor = class(TInterfacedObject, IActor)
  private
  protected
    procedure Stop;
  public
    Owner: TObject;
    Ref: TActorRef;
    Parent: TActorRef;
    Mailbox: TActorMailBox;
    procedure Receive(const M: TActorMessage;
                      S: TActorScheduler;
                      Sender: TActorRef); virtual;
    procedure SendTo(Receiver: TActorRef; const M: TActorMessage);
    constructor Create(Owner: TObject); virtual;
  end;

implementation

uses
  Actor.Environment;

procedure TActor.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
end;

procedure TActor.SendTo(Receiver: TActorRef; const M: TActorMessage);
begin
  (Owner as TActorEnvironment).SendTo(Ref, Receiver, M);
end;

procedure TActor.Stop;
begin
  (Owner as TActorEnvironment).RemoveActor(Ref);
end;

constructor TActor.Create(Owner: TObject);
begin
  Assert(Owner is TActorEnvironment);
  Self.Owner := Owner;
end;

end.
