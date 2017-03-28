unit SessionClasses;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  {** TSession is the base class, from which all sessions are derived. }
  TSession = class
  public
    constructor Create; virtual;
  end;
  TSessionClass = class of TSession;

  {** The TSessionContainer class holds the sessions in both the list and the hash table. It is used
   internally by the session manager only. Note that when creating a session container, it will
   automatically create a session object. When the container is freed, the session object will
   automatically be freed as well. }
  TSessionContainer = class
  protected
    ID: Cardinal;
    Session: TSession;
    Older: TSessionContainer;
    Newer: TSessionContainer;
    LastAccess: TDateTime;
  public
    constructor Create(SessionClass: TSessionClass);
    destructor Destroy; override;
  end;

  {** This class implements a hash table used to keep TSessionContainer objects, allowing quick
   access to each object, given its session ID. Note that the constructor's OwnsObjects parameter
   dictates whether inserted session containers are automatically freed by this object when its
   Remove or Clear method is called. }
  TSessionHashTable = class
  protected
    _OwnsObjects: Boolean;
    Entries: Array[Word] of TList;
    function Hash(ID: Cardinal): Word;
  public
    function Get(ID: Cardinal): TSessionContainer;
    function Insert(ID: Cardinal; aSessionContainer: TSessionContainer): Boolean;
    function Remove(ID: Cardinal): Boolean;
    procedure Clear;
    constructor Create(OwnsObjects: Boolean);
    destructor Destroy; override;
  end;

  {** This class implements the time list, where session containers are kept in time order. }
  TTimeList = class
  protected
    Oldest: TSessionContainer;
    Newest: TSessionContainer;
  public

    {** Inserts the given container into the time list, as the "newest" object. Also updates the
     session container's LastAccessTime. }
    procedure Add(SessionContainer: TSessionContainer);

    {** Removes the given container from the time list. }
    procedure Remove(SessionContainer: TSessionContainer);

    {** Moves the given container to the beginning of the time list, as the "newest" object. Also
     updates the session container's LastAccessTime. }
    procedure Update(SessionContainer: TSessionContainer);

    procedure Clear;
    constructor Create;
  end;

  {** The TSessionManager class manages sessions. It exposes an interface letting a user request a
   new session, retrieve an existing session or remove a session. It also automatically removes
   sessions that have not been used in a while. }
  TSessionManager = class
  protected
    EvictionTimer: TTimer;
    SessionClass: TSessionClass;
    SessionTimeList: TTimeList;
    SessionHashTable: TSessionHashTable;
    EvictionTimeSeconds: Cardinal;
    procedure EvictionTimerTimer(Sender: TObject);
    function GenerateSessionID: Cardinal; virtual;
  public

    {** Creates a new session with a unique ID, and returns the ID. }
    function NewSession: Cardinal;

    {** Returns the session with given ID if found, otherwise returns nil. If a session was found,
     it is also updated so that it is now the "newest", i.e. most recently accessed session. }
    function GetSession(aID: Cardinal): TSession;

    {** Removes a session with given ID. Returns True if the session was deleted and False if no
     such session could be found. }
    function RemoveSession(aID: Cardinal): Boolean;

    {** Removes all sessions from the tree and the list, freeing all objects. }
    procedure Clear;

    constructor Create(aSessionClass: TSessionClass; aEvictionTimeSeconds: Cardinal = 600);
    destructor Destroy; override;
  end;

implementation

{----------  TSession ---------------------------------------------------------}

constructor TSession.Create;
begin
end;

{---------- TSessionContainer -------------------------------------------------}

constructor TSessionContainer.Create(SessionClass: TSessionClass);
begin
  Session := SessionClass.Create;
  Older := nil;
  Newer := nil;
end;

destructor TSessionContainer.Destroy;
begin
  Session.Free;
  inherited;
end;

{---------- TSessionHashTable ---------------------------------------------------------------------}

function TSessionHashTable.Hash(ID: Cardinal): Word;
begin
  Result := ID mod High(Word);
end;

function TSessionHashTable.Get(ID: Cardinal): TSessionContainer;
var
  i: Integer;
  Entry: TList;
begin
  Result := nil;
  Entry := Entries[Hash(ID)];
  if Assigned(Entry) then
    for i := 0 to Entry.Count-1 do
      if TSessionContainer(Entry[i]).ID = ID then
      begin
        Result := TSessionContainer(Entry[i]);
        Break;
      end;
end;

function TSessionHashTable.Insert(ID: Cardinal; aSessionContainer: TSessionContainer): Boolean;
var
  H: Word;
  Entry: TList;
begin
  Result := False;
  if Assigned(aSessionContainer) and (not Assigned(Get(ID))) then
  begin
    H := Hash(ID);
    Entry := Entries[H];
    if not Assigned(Entry) then
    begin
      Entry := TList.Create;
      Entries[H] := Entry;
    end;
    aSessionContainer.ID := ID;
    Entry.Add(aSessionContainer);
    Result := True;
  end;
end;

function TSessionHashTable.Remove(ID: Cardinal): Boolean;
var
  i: Integer;
  Entry: TList;
  SC: TSessionContainer;
begin
  Result := False;
  Entry := Entries[Hash(ID)];
  if Assigned(Entry) then
    for i := 0 to Entry.Count-1 do
    begin
      SC := TSessionContainer(Entry[i]);
      if SC.ID = ID then
      begin
        if _OwnsObjects then SC.Free;
        Entry.Delete(i);
        Result := True;
        Break;
      end;
    end;
end;

procedure TSessionHashTable.Clear;
var
  i: Word;
  j: Integer;
begin
  for i := 0 to High(Word)-1 do
    if Assigned(Entries[i]) then
    begin
      if _OwnsObjects then
        for j := 0 to Entries[i].Count-1 do
          TSessionContainer(Entries[i].Items[j]).Free;
      Entries[i].Free;
      Entries[i] := nil;
    end;
end;

constructor TSessionHashTable.Create(OwnsObjects: Boolean);
var
  i: Word;
begin
  _OwnsObjects := OwnsObjects;
  for i := 0 to High(Word)-1 do
    Entries[i] := nil;
end;

destructor TSessionHashTable.Destroy;
begin
  Clear;
  inherited;
end;

{---------- TTimeList -----------------------------------------------------------------------------}

procedure TTimeList.Add(SessionContainer: TSessionContainer);
begin
  if Assigned(Newest) then
  begin
    Newest.Newer := SessionContainer;
    SessionContainer.Older := Newest;
  end;
  Newest := SessionContainer;
  SessionContainer.Newer := nil;
  if not Assigned(Oldest) then
    Oldest := Newest;
  SessionContainer.LastAccess := Now;
end;

procedure TTimeList.Remove(SessionContainer: TSessionContainer);
begin
  if (SessionContainer = Newest) and (SessionContainer = Oldest) then
  begin
    Oldest := nil;
    Newest := nil;
  end
  else if SessionContainer = Newest then
  begin
    Newest := SessionContainer.Older;
    Newest.Newer := nil;
  end
  else if SessionContainer = Oldest then
  begin
    Oldest := SessionContainer.Newer;
    Oldest.Older := nil;
  end
  else
  begin
    SessionContainer.Newer.Older := SessionContainer.Older;
    SessionContainer.Older.Newer := SessionContainer.Newer;
  end;
end;

procedure TTimeList.Update(SessionContainer: TSessionContainer);
begin
  Remove(SessionContainer);
  Add(SessionContainer);
end;

procedure TTimeList.Clear;
begin
  Oldest := nil;
  Newest := nil;
end;

constructor TTimeList.Create;
begin
  Oldest := nil;
  Newest := nil;
end;

{---------- TSessionManager ---------------------------------------------------}

procedure TSessionManager.EvictionTimerTimer(Sender: TObject);
begin
  repeat
    if not Assigned(SessionTimeList.Oldest) then
      Break;
    if (Now - SessionTimeList.Oldest.LastAccess) > EvictionTimeSeconds / SecsPerDay then
      RemoveSession(SessionTimeList.Oldest.ID)
    else
      Break;
  until False; {Will break out}
end;

function TSessionManager.GenerateSessionID: Cardinal;
begin
  Result := Random(High(Integer))
end;

procedure TSessionManager.Clear;
begin
  SessionTimeList.Clear;
  SessionHashTable.Clear; {Will also free all sessions and session containers}
end;

function TSessionManager.NewSession: Cardinal;
var
  SC: TSessionContainer;
begin
  SC := TSessionContainer.Create(SessionClass);
  repeat
    Result := GenerateSessionID;
  until SessionHashTable.Insert(Result, SC);
  SessionTimeList.Add(SC);
end;

function TSessionManager.GetSession(aID: Cardinal): TSession;
var
  SC: TSessionContainer;
begin
  SC := SessionHashTable.Get(aID);
  if Assigned(SC) then
  begin
    Result := SC.Session;
    SessionTimeList.Update(SC);
  end
  else
    Result := nil;
end;

function TSessionManager.RemoveSession(aID: Cardinal): Boolean;
var
  SC: TSessionContainer;
begin
  SC := SessionHashTable.Get(aID);
  if Assigned(SC) then
  begin
    SessionTimeList.Remove(SC);
    SessionHashTable.Remove(aID);
    Result := True;
  end
  else
    Result := False;
end;

constructor TSessionManager.Create(aSessionClass: TSessionClass; aEvictionTimeSeconds: Cardinal);
begin
  SessionClass := aSessionClass;
  EvictionTimeSeconds := aEvictionTimeSeconds;
  EvictionTimer := TTimer.Create(nil);
  EvictionTimer.Interval := 1000;
  {$ifdef FPC}
  EvictionTimer.OnTimer := @EvictionTimerTimer;
  {$else}
  EvictionTimer.OnTimer := EvictionTimerTimer;
  {$endif}
  EvictionTimer.Enabled := True;
  SessionTimeList := TTimeList.Create;
  SessionHashTable := TSessionHashTable.Create(True);
end;

destructor TSessionManager.Destroy;
begin
  Clear;
  EvictionTimer.Free;
  SessionHashTable.Free;
  SessionTimeList.Free;
  inherited;
end;

end.

