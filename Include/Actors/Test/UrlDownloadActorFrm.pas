unit UrlDownloadActorFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Actor.Types, Actor.Actor, Actor.Environment, Actor.Scheduler, StdCtrls, OleCtrls, SHDocVw;

type
  { This actor adds the received string to the memo. }
  TPresenter = class(TActor)
  private
    FMemo: TMemo;
  public
    property Memo: TMemo read FMemo write FMemo;
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;

  TDownloader = class(TActor)
  protected
    Presenter: TActorRef;
  public
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;


  TForm16 = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  Form16: TForm16;

implementation

uses
  idHTTP;

{$R *.dfm}

function GetURLAsString(const aURL: string): string;
var
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create(nil);
  try
    lHTTP.HandleRedirects := True;
    Result := lHTTP.Get(aURL);
  finally
    lHTTP.Free;
  end;
end;

{ TPresenter }

procedure TPresenter.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  S.Block(procedure begin Memo.Lines.Add(M); end);
end;

{ TDownloader }

procedure TDownloader.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
var
  Body: String;
begin
  try
    Body := GetURLAsString(M);
  except
    on E: Exception do
      Body := E.ClassName + ' with message "' + E.Message + '"';
  end;
  ActorEnvironment.SendTo(''{Self}, Presenter, Body);
end;

procedure TForm16.Button1Click(Sender: TObject);
var
  Downloader, Presenter: TActorRef;
begin
  Presenter := ActorEnvironment.ActorOf(TPresenter, '',
    procedure(A: IActor)
    begin
      (A as TPresenter).Memo := Memo1;
    end);

  Downloader := ActorEnvironment.ActorOf(TDownloader, '',
    procedure(A: IActor)
    begin
      (A as TDownloader).Presenter := Presenter;
    end);

  ActorEnvironment.SendTo('', Downloader, Edit1.Text);
end;

end.
