unit Actor.Remote.Intf;

interface

uses
  Actor.Types, Actor.Actor;

type
  TPeerEvent = reference to procedure(const PeerIP: String; PeerPort: Cardinal);
  TActorEvent = reference to procedure(const PeerIP: String; PeerPort: Cardinal;
                                       const ActorClass: String);

  IActorRemote = interface
    function GetOnPeerAdded: TPeerEvent;
    procedure SetOnPeerAdded(Value: TPeerEvent);
    function GetOnPeerRemoved: TPeerEvent;
    procedure SetOnPeerRemoved(Value: TPeerEvent);
    function GetOnActorRegistered: TActorEvent;
    procedure SetOnActorRegistered(Value: TActorEvent);
    function GetOnActorUnregistered: TActorEvent;
    procedure SetOnActorUnregistered(Value: TActorEvent);

    property OnPeerAdded: TPeerEvent
      read GetOnPeerAdded
      write SetOnPeerAdded;
    property OnPeerRemoved: TPeerEvent
      read GetOnPeerRemoved
      write SetOnPeerRemoved;
    property OnActorRegistered: TActorEvent
      read GetOnActorRegistered
      write SetOnActorRegistered;
    property OnActorUnregistered: TActorEvent
      read GetOnActorUnregistered
      write SetOnActorUnregistered;

    procedure Activate(const IP: String; Port: Cardinal);
    procedure Deactivate();

    function Open(const PeerIP: String; PeerPort: Cardinal): Boolean;
    function Close(const PeerIP: String; PeerPort: Cardinal): Boolean;

    function Register(const PeerIP: String; PeerPort: Cardinal; ActorClass: TActorClass): Boolean;
    function UnRegister(const PeerIP: String; PeerPort: Cardinal; ActorClass: TActorClass): Boolean;

    function ActorOf(const PeerIP: String; PeerPort: Cardinal; ActorClass: TActorClass;
                     const Parent: TActorRef=''): TActorRef;

    procedure SendTo(const PeerIP: String; PeerPort: Cardinal;
                     const Sender, Receiver: TActorRef; const M: TActorMessage);

  end;

implementation

end.
