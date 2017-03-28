unit TestSessionClasses;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, SessionClasses;

type
  {** This class implements a session manager suitable for test projects. If RandomSessionIDs is
   False, it doesn't generate IDs randomly, but starts at 10000 and increments by 10000 each time.
   In addition to this, it provides methods for printing the contents of the session manager. }
  TTestSessionManager = class(TSessionManager)
  private
    FRandomSessionIDs: Boolean;
  protected
    HighestSessionID: Cardinal;
    function GenerateSessionID: Cardinal; override;
  public
    property RandomSessionIDs: Boolean  read FRandomSessionIDs write FRandomSessionIDs;
    procedure DrawHashTableContents(Strings: TStrings);
    procedure DrawHashTableStatistics(Strings: TStrings);
    function DrawTimeList: String;
  end;

  TTestSession = class(TSession)
  end;

  TTestSessionContainer = class(TSessionContainer)
  end;

  TTestTimeList = class(TTimeList)
  end;

  TTestSessionHashTable = class(TSessionHashTable)
  end;

implementation

uses
  Math;

function TTestSessionManager.GenerateSessionID: Cardinal;
begin
  if RandomSessionIDs then
    Result := inherited
  else
  begin
    Inc(HighestSessionID, 10000);
    Result := HighestSessionID;
  end;
end;

procedure TTestSessionManager.DrawHashTableContents(Strings: TStrings);
var
  i: Word;
  j: Integer;
  S: String;
  List: TList;
begin
  Strings.Clear;
  for i := 0 to High(Word)-1 do
  begin
    List := TTestSessionHashTable(SessionHashTable).Entries[i];
    if Assigned(List) and (List.Count > 0) then
    begin
      S := IntToStr(i) + ': ';
      for j := 0 to List.Count-1 do
        S := S + IntToStr(TTestSessionContainer(List[j]).ID) + ' ';
      Strings.Add(S);
    end;
  end;
end;

procedure TTestSessionManager.DrawHashTableStatistics(Strings: TStrings);
var
  i, j: Word;
  C: Integer;
  Avg: Double;
  Sum: Cardinal;
  Sums: Array[0..100] of Cardinal;
  ItemCounts: Array[0..100] of Cardinal;
begin
  Strings.Clear;
  Sum := 0;
  Avg := 0;
  for i := 0 to 100 do
  begin
    Sums[i] := 0;
    ItemCounts[i] := 0;
  end;
  for i := 0 to High(Word)-1 do
    if Assigned(TTestSessionHashTable(SessionHashTable).Entries[i]) then
    begin
      C := TTestSessionHashTable(SessionHashTable).Entries[i].Count;
      Inc(ItemCounts[Min(C, 100)]);
    end;
  for i := 1 to 100 do
  begin
    if ItemCounts[i] > 0 then
      Strings.Add(Format('%d keys hold %d items', [ItemCounts[i], i]));
    for j := i to 100 do
      Inc(Sums[i], ItemCounts[j]);
    Inc(Sum, Sums[i]);
  end;
  for i := 1 to 100 do
  begin
    if Sums[i] > 0 then
      Strings.Add(Format('Sums[%d]: %d', [i, Sums[i]]));
    Avg := Avg + i * Sums[i];
  end;
  Avg := Avg / Sum;
  Strings.Add(Format('Sum: %d', [Sum]));
  Strings.Add(Format('Avg: %f', [Avg]));
end;

function TTestSessionManager.DrawTimeList: String;
var
  SC: TSessionContainer;
begin
  Result := '';
  SC := TTestTimeList(SessionTimeList).Oldest;
  while Assigned(SC) do
  begin
    Result := Result + IntToStr(TTestSessionContainer(SC).ID) + ', ';
    SC := TTestSessionContainer(SC).Newer;
  end;
  if Result <> '' then
    Result := Copy(Result, 1, Length(Result)-2);
end;

end.

