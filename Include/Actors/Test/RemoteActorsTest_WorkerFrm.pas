unit RemoteActorsTest_WorkerFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RemoteActorsTest_Actors, StdCtrls, Actor.Types, Actor.Actor, Actor.Environment;

type
  TRemoteActorsTest_WorkerForm = class(TForm)
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    Presenter: TActorRef;
    Environment: TActorEnvironment;
  public
    procedure Execute(Port: Cardinal);
  end;

var
  RemoteActorsTest_WorkerForm: TRemoteActorsTest_WorkerForm;

implementation

{$R *.dfm}

uses
  Actor.Remote.Remote;

{ TRemoteActorsTest_WorkerForm }

procedure TRemoteActorsTest_WorkerForm.Execute(Port: Cardinal);
begin
  Show;

  { Create environment and activate it on the given port }
  Caption := 'Worker@'+IntToStr(Port);
  Environment := TActorEnvironment.Create(2);
  Environment.Remote := TActorRemote.Create(Environment);
  Environment.Remote.Activate('127.0.0.1', Port);

  { Let the central environment know that I support the TWorker actor }
  Environment.Remote.Open('127.0.0.1', 9000);
  Environment.Remote.Register('127.0.0.1', 9000, TWorker);

  Presenter := Environment.ActorOf(TPresenter, '',
    procedure(A: IActor)
    begin
      (A as TPresenter).Memo := Self.Memo;
    end);
  Environment.SendTo('', Presenter, 'Environment listens to port ' + IntToStr(Port));
end;

procedure TRemoteActorsTest_WorkerForm.FormCreate(Sender: TObject);
begin
  Environment := nil;
end;

procedure TRemoteActorsTest_WorkerForm.FormDestroy(Sender: TObject);
begin
  Environment.Remote.Deactivate;
  Environment.Free;
end;

end.
