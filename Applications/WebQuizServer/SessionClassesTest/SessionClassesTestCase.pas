unit SessionClassesTestCase;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

{ Note: a session is completely controlled by the TSessionManager object. You may
  access its properties only immediately after GetSession. Keeping a reference
  to the session, the way we do here, is dangerous under normal circumstances,
  since the session manager may have evicted the session when you try to use it. }

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  fpcunit, testregistry,
  {$else}
  TestFramework,
  {$endif}
  SessionClasses, TestSessionClasses;

type
  { The following classes allow us to access protected members of their ancestors }
  TTestSession_ = class(TTestSession) end;
  TTestSessionContainer_ = class(TTestSessionContainer) end;
  TTestTimeList_ = class(TTestTimeList) end;
  TTestSessionHashTable_ = class(TTestSessionHashTable) end;
  TTestSessionManager_ = class(TTestSessionManager) end;

  { TSessionClassesTestCase }

  TSessionClassesTestCase= class(TTestCase)
  protected
    SessionManager: TTestSessionManager_;
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure OneSessionTest;
    procedure OneSessionEvictionTest;
    procedure MultipleSessionsTest;
    procedure MultipleSessionsEvictionTest;
  end;

implementation

uses
  Forms;

procedure TSessionClassesTestCase.OneSessionTest;
var
  ID: Cardinal;
  Session: TTestSession;
begin
  CheckNotNull(SessionManager);
  CheckNull(SessionManager.GetSession(10000));
  ID := SessionManager.NewSession;
  Session := SessionManager.GetSession(ID) as TTestSession;
  CheckNotNull(Session);
  CheckIs(Session, TTestSession);
  CheckEquals(10000, ID);
  Check(not SessionManager.RemoveSession(20000), 'RemoveSession(20000) should return False');
  Session := SessionManager.GetSession(ID) as TTestSession;
  CheckNotNull(Session);
  SessionManager.RemoveSession(10000);
  Session := SessionManager.GetSession(ID) as TTestSession;
  CheckNull(Session);
end;

procedure TSessionClassesTestCase.OneSessionEvictionTest;
var
  i: Integer;
begin
  SessionManager.NewSession;

  { Access the session a few times, and make sure it is not evicted }
  for i := 0 to 15 do
  begin
    Sleep(200);
    Application.ProcessMessages;
    CheckNotNull(SessionManager.GetSession(10000), 'Session has been evicted');
  end;

  { Make sure the session is evicted after too long inactivity}
  Sleep(3000);
  Application.ProcessMessages;
  CheckNull(SessionManager.GetSession(10000), 'Session has not been evicted');
end;

procedure TSessionClassesTestCase.MultipleSessionsTest;

  function GetOldest: TSessionContainer;
  begin
    Result := TTestTimeList_(SessionManager.SessionTimeList).Oldest;
  end;

  function GetNewest: TSessionContainer;
  begin
    Result := TTestTimeList_(SessionManager.SessionTimeList).Newest;
  end;

  function GetHashItem(EntryIndex, ListIndex: Integer): TSessionContainer;
  begin
    Result := TSessionContainer(TTestSessionHashTable_(SessionManager.SessionHashTable).
      Entries[EntryIndex].Items[ListIndex]);
  end;

  procedure CompareSessions(aSessionContainer: TSessionContainer; aSession: TSession);
  begin
    Check(TTestSessionContainer_(aSessionContainer).Session = aSession);
  end;

var
  S1, S2, S3: TTestSession;
  ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8, ID9, ID10: Cardinal;
begin
  SessionManager.EvictionTimer.Enabled := False;

  ID1 := SessionManager.NewSession;
  ID2 := SessionManager.NewSession;
  ID3 := SessionManager.NewSession;
  CheckEquals(10000, ID1);
  CheckEquals(20000, ID2);
  CheckEquals(30000, ID3);
  S1 := SessionManager.GetSession(ID1) as TTestSession;
  S2 := SessionManager.GetSession(ID2) as TTestSession;
  S3 := SessionManager.GetSession(ID3) as TTestSession;

  CheckEquals('10000, 20000, 30000', SessionManager.DrawTimeList);
  CompareSessions(GetHashItem(10000, 0), S1);
  CompareSessions(GetHashItem(20000, 0), S2);
  CompareSessions(GetHashItem(30000, 0), S3);
  CompareSessions(GetOldest, S1);
  CompareSessions(GetNewest, S3);

  SessionManager.GetSession(10);
  CheckEquals('10000, 20000, 30000', SessionManager.DrawTimeList);
  CompareSessions(GetHashItem(10000, 0), S1);
  CompareSessions(GetHashItem(20000, 0), S2);
  CompareSessions(GetHashItem(30000, 0), S3);
  CompareSessions(GetOldest, S1);
  CompareSessions(GetNewest, S3);

  SessionManager.GetSession(ID1);
  CheckEquals('20000, 30000, 10000', SessionManager.DrawTimeList);
  CompareSessions(GetHashItem(10000, 0), S1);
  CompareSessions(GetHashItem(20000, 0), S2);
  CompareSessions(GetHashItem(30000, 0), S3);
  CompareSessions(GetOldest, S2);
  CompareSessions(GetNewest, S1);

  SessionManager.GetSession(ID1);
  CheckEquals('20000, 30000, 10000', SessionManager.DrawTimeList);
  CompareSessions(GetHashItem(10000, 0), S1);
  CompareSessions(GetHashItem(20000, 0), S2);
  CompareSessions(GetHashItem(30000, 0), S3);
  CompareSessions(GetOldest, S2);
  CompareSessions(GetNewest, S1);

  SessionManager.GetSession(ID2);
  CheckEquals('30000, 10000, 20000', SessionManager.DrawTimeList);
  CompareSessions(GetHashItem(10000, 0), S1);
  CompareSessions(GetHashItem(20000, 0), S2);
  CompareSessions(GetHashItem(30000, 0), S3);
  CompareSessions(GetOldest, S3);
  CompareSessions(GetNewest, S2);

  SessionManager.RemoveSession(ID1);
  CheckNull(SessionManager.GetSession(ID1));
  CheckEquals('30000, 20000', SessionManager.DrawTimeList);
  CompareSessions(GetHashItem(20000, 0), S2);
  CompareSessions(GetHashItem(30000, 0), S3);
  CompareSessions(GetOldest, S3);
  CompareSessions(GetNewest, S2);

  ID1 := SessionManager.NewSession;
  ID4 := SessionManager.NewSession;
  ID5 := SessionManager.NewSession;
  ID6 := SessionManager.NewSession;
  ID7 := SessionManager.NewSession;
  ID8 := SessionManager.NewSession;
  ID9 := SessionManager.NewSession;
  ID10 := SessionManager.NewSession;
  CheckEquals('30000, 20000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000', SessionManager.DrawTimeList);
  CompareSessions(GetHashItem(20000, 0), S2);
  CompareSessions(GetHashItem(30000, 0), S3);
  CompareSessions(GetHashItem(40000, 0), SessionManager.GetSession(ID1));
  CompareSessions(GetHashItem(50000, 0), SessionManager.GetSession(ID4));
  CompareSessions(GetHashItem(60000, 0), SessionManager.GetSession(ID5));
  CompareSessions(GetHashItem(4465, 0), SessionManager.GetSession(ID6));
  CompareSessions(GetHashItem(14465, 0), SessionManager.GetSession(ID7));
  CompareSessions(GetHashItem(24465, 0), SessionManager.GetSession(ID8));
  CompareSessions(GetHashItem(34465, 0), SessionManager.GetSession(ID9));
  CompareSessions(GetHashItem(44465, 0), SessionManager.GetSession(ID10));
  CompareSessions(GetOldest, S3);
  CompareSessions(GetNewest, SessionManager.GetSession(ID10));
end;

procedure TSessionClassesTestCase.MultipleSessionsEvictionTest;
var
  i: Integer;
begin
  SessionManager.NewSession;
  SessionManager.NewSession;
  SessionManager.NewSession;
  CheckNotNull(SessionManager.GetSession(10000), 'Session has been evicted');
  CheckNotNull(SessionManager.GetSession(20000), 'Session has been evicted');
  CheckNotNull(SessionManager.GetSession(30000), 'Session has been evicted');

  { Access two of the sessions a few times, and make sure they are not evicted }
  for i := 0 to 15 do
  begin
    Sleep(200);
    Application.ProcessMessages;
    CheckNotNull(SessionManager.GetSession(10000), 'Session has been evicted');
    CheckNotNull(SessionManager.GetSession(20000), 'Session has been evicted');
  end;

  { Make sure the third session is evicted }
  Application.ProcessMessages;
  CheckNull(SessionManager.GetSession(30000), 'Session has not been evicted');
  CheckNotNull(SessionManager.GetSession(10000), 'Session has been evicted');
  CheckNotNull(SessionManager.GetSession(20000), 'Session has been evicted');
end;

procedure TSessionClassesTestCase.SetUp;
begin
  SessionManager := TTestSessionManager_.Create(TTestSession, 2);
end; 

procedure TSessionClassesTestCase.TearDown;
begin
  SessionManager.Free;
end; 

initialization
  {$ifdef FPC}
  RegisterTest(TSessionClassesTestCase);
  {$else}
  RegisterTests('', [TSessionClassesTestCase.Suite]);
  {$endif}
end.

