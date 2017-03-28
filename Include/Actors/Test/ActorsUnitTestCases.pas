unit ActorsUnitTestCases;

interface

uses
  Generics.Collections, Classes, TestFramework,
  Actor.Remote.Intf, Actor.Actor, Actor.Types,
  Actor.Environment, Actor.Scheduler, Actor.Remote.Remote;

type
  TTester = class(TActor)
  protected
    M: TActorMessage;
  public
    function Probe(MaxTimeOut: Cardinal = 1000): String;
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;

  TCounter = class(TActor)
  protected
    Value: Integer;
  public
    procedure Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef); override;
  end;

  TActorsTestCases = class(TTestCase)
  strict private
    Tester: TTester;
    TesterRef: TActorRef;
    DefaultEnvironment: TActorEnvironment;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SimpleTest;
    procedure SimpleRemoteTest;
  end;

implementation

uses
  SysUtils;

{ TCounter }

procedure TCounter.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  if M = 'INC' then
    Inc(Value);
  if M = 'DEC' then
    Dec(Value);
  if M = 'ASK' then
    SendTo(Sender, IntToStr(Value))
end;

{ TTester }

function TTester.Probe(MaxTimeOut: Cardinal): String;
begin
  Sleep(100);
  Result := M;
end;

procedure TTester.Receive(const M: TActorMessage; S: TActorScheduler; Sender: TActorRef);
begin
  Self.M := M;
end;

{ TActorsTestCases }

procedure TActorsTestCases.SetUp;
begin
  DefaultEnvironment := TActorEnvironment.Create(4);
  TesterRef := DefaultEnvironment.ActorOf(TTester, '',
                                          procedure(A: IActor)
                                          begin
                                            Tester := (A as TTester);
                                          end);
end;

procedure TActorsTestCases.SimpleTest;
var
  AR: TActorRef;
begin
  AR := DefaultEnvironment.ActorOf(TCounter);
  DefaultEnvironment.SendTo(TesterRef, AR, 'ASK');
  CheckEquals('0', Tester.Probe);
  DefaultEnvironment.SendTo(TesterRef, AR, 'INC');
  DefaultEnvironment.SendTo(TesterRef, AR, 'INC');
  DefaultEnvironment.SendTo(TesterRef, AR, 'INC');
  DefaultEnvironment.SendTo(TesterRef, AR, 'ASK');
  CheckEquals('3', Tester.Probe);
end;

procedure TActorsTestCases.SimpleRemoteTest;
var
  AR: TActorRef;
  RemoteEnvironment: TActorEnvironment;
begin
  DefaultEnvironment.Remote := TActorRemote.Create(DefaultEnvironment);
  DefaultEnvironment.Remote.Activate('127.0.0.1', 9000);

  RemoteEnvironment := TActorEnvironment.Create(4);
  RemoteEnvironment.Remote := TActorRemote.Create(RemoteEnvironment);
  try
    RemoteEnvironment.Remote.Activate('127.0.0.1', 9001);
    RemoteEnvironment.Remote.Open('127.0.0.1', 9000);
    RemoteEnvironment.Remote.Register('127.0.0.1', 9000, TCounter);

    AR := DefaultEnvironment.Remote.ActorOf('127.0.0.1', 9001, TCounter);
    DefaultEnvironment.SendTo(TesterRef, AR, 'ASK');
    CheckEquals('0', Tester.Probe);
    DefaultEnvironment.SendTo(TesterRef, AR, 'INC');
    DefaultEnvironment.SendTo(TesterRef, AR, 'INC');
    DefaultEnvironment.SendTo(TesterRef, AR, 'INC');
    DefaultEnvironment.SendTo(TesterRef, AR, 'ASK');
    CheckEquals('3', Tester.Probe);
  finally
    RemoteEnvironment.Free;
  end;
end;

procedure TActorsTestCases.TearDown;
begin
  DefaultEnvironment.Free;
end;

initialization
  RegisterTest(TActorsTestCases.Suite);

end.

