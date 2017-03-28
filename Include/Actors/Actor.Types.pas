unit Actor.Types;

interface

type
  IActor = interface end;
  TActorRef = String;
  TActorMessage = String;
  TActorMessageEnvelope = record
    M: TActorMessage;
    Sender: TActorRef;
    Receiver: IActor;
  end;
  PActorMessageEnvelope = ^TActorMessageEnvelope;

implementation

end.
