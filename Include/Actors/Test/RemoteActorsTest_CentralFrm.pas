unit RemoteActorsTest_CentralFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Actor.Types, Actor.Actor, Actor.Environment, Actor.Scheduler,
  Actor.Remote.Client{, Actor.Remote.Communicator};

type
  TRemoteActorsTest_CentralForm = class(TForm)
    Memo: TMemo;
    SpinEdit: TSpinEdit;
    ActivateButton: TButton;
    CreateActorButton: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ActivateButtonClick(Sender: TObject);
    procedure CreateActorButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  protected
    Presenter: TActorRef;
    Environment: TActorEnvironment;
    procedure PeerAdded(const PeerIP: String; PeerPort: Cardinal);
    procedure ActorRegistered(const PeerIP: String; PeerPort: Cardinal; const ActorClass: String);
  end;

var
  RemoteActorsTest_CentralForm: TRemoteActorsTest_CentralForm;

implementation

{$R *.dfm}

uses
  RemoteActorsTest_Actors, RemoteActorsTest_WorkerFrm, Actor.Remote.Remote;

procedure TRemoteActorsTest_CentralForm.PeerAdded(const PeerIP: String; PeerPort: Cardinal);
begin
  Environment.SendTo('', Presenter, 'New peer at port ' + IntToStr(PeerPort));
end;

procedure TRemoteActorsTest_CentralForm.ActorRegistered(const PeerIP: String; PeerPort: Cardinal;
  const ActorClass: String);
begin
  Environment.SendTo('', Presenter, Format('Environment at %s:%d supports %s',
    [PeerIP, PeerPort, ActorClass]));
end;

procedure TRemoteActorsTest_CentralForm.Button1Click(Sender: TObject);
begin
  Environment.SendTo('', ListBox1.Items[ListBox1.ItemIndex], 'INC');
end;

procedure TRemoteActorsTest_CentralForm.Button2Click(Sender: TObject);
begin
  Environment.SendTo('', ListBox1.Items[ListBox1.ItemIndex], 'DEC');
end;

procedure TRemoteActorsTest_CentralForm.Button3Click(Sender: TObject);
begin
  Environment.SendTo(Presenter, ListBox1.Items[ListBox1.ItemIndex], 'ASK');
end;

procedure TRemoteActorsTest_CentralForm.FormCreate(Sender: TObject);
begin
  Environment := TActorEnvironment.Create(2);
  Environment.Remote := TActorRemote.Create(Environment);
  Environment.Remote.Activate('127.0.0.1', 9000);
  Environment.Remote.OnPeerAdded := PeerAdded;
  Environment.Remote.OnPeerRemoved := nil; // todo
  Environment.Remote.OnActorRegistered := ActorRegistered;
  Environment.Remote.OnActorUnregistered := nil; // todo
  Presenter := Environment.ActorOf(TPresenter, '',
    procedure(A: IActor)
    begin
      (A as TPresenter).Memo := Self.Memo;
    end);
  Environment.SendTo('', Presenter, 'Environment listens to port 9000');
end;

procedure TRemoteActorsTest_CentralForm.ActivateButtonClick(Sender: TObject);
begin
  with TRemoteActorsTest_WorkerForm.Create(Self) do
    Execute(SpinEdit.Value);
end;

procedure TRemoteActorsTest_CentralForm.CreateActorButtonClick(Sender: TObject);
var
  ActorRef: String;
begin
  ActorRef := Environment.Remote.ActorOf('127.0.0.1', SpinEdit.Value, TWorker, Presenter);
  ListBox1.Items.Add(ActorRef);
  //Environment.SendTo('', Presenter, 'New actor "' + ActorRef + '"');
  //Environment.SendTo('', ActorRef, 'Hello and welcome!');

  // OBS! Kolla att workern kan skicka medd. till sin parent!
end;

end.
