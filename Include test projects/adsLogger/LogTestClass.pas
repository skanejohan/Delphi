unit LogTestClass;

interface

uses
  Classes, Contnrs, TestFramework, adsLoggerClass;

type
  TLogTest = class
  protected
    TestCase: TTestCase;
    ReferenceLogFiles: TStringList;
    ReferenceLists: TObjectList; {Of TStringList}
    procedure CompareLineInternal(const Expected, Actual: String);
  public
    procedure StartLogging;

    function AddSubscription(const LogFile: String; LogTypes: TadsLogTypes;
      MaxFileSize: Integer = 1024; BackupFiles: Boolean = True): Integer;

    procedure AddToReferenceLists(Sender: TObject; LogType: TadsLogType;
      const Description: String; IDs: Array of Integer);

    procedure Log(Sender: TObject; LogType: TadsLogType; const Description: String;
      IDs: Array of Integer);

    procedure StartCompare;

    procedure VerifyNumberOfLines(ID, ExpectedLines: Integer);

    procedure CompareLine(ID, LineNo: Integer; const Expected: String);

    procedure Compare(ID: Integer; NoOfLines: Integer);

    procedure Stop;

    constructor Create(aTestCase: TTestCase);
    destructor Destroy; override;
   end{class};

implementation

uses
  SysUtils;

procedure TLogTest.CompareLineInternal(const Expected, Actual: String);
var
  Line: String;
begin
  Line := Copy(Actual, 25, MAXINT); {Strip the time part; we can't verify that}
  TestCase.CheckEquals(Expected, Line);
end{procedure};

procedure TLogTest.StartLogging;
begin
  FreeAndNil(Logger);
  Logger := TadsLogger.Create;
end{procedure};

function TLogTest.AddSubscription(const LogFile: String;
  LogTypes: TadsLogTypes; MaxFileSize: Integer = 1024;
  BackupFiles: Boolean = True): Integer;
begin
  DeleteFile(LogFile);
  ReferenceLogFiles.Add(LogFile);
  ReferenceLists.Add(TStringList.Create);
  Logger.AddSubscription(LogFile, LogTypes, MaxFileSize, BackupFiles);
  Result := ReferenceLists.Count-1;
end{function};

procedure TLogTest.AddToReferenceLists(Sender: TObject; LogType: TadsLogType;
  const Description: String; IDs: Array of Integer);
var
  i: Integer;
  CName: String;
begin
  if Assigned(Sender) then
    CName := Sender.ClassName
  else
    CName := '<<nil>>';
  for i := Low(IDs) to High(IDs) do
    (ReferenceLists[IDs[i]] as TStringList).Add(LogTypeNames[LogType] + ';' +
      CName + ';' + Description);
end{function};

procedure TLogTest.Log(Sender: TObject; LogType: TadsLogType;
  const Description: String; IDs: Array of Integer);
begin
  Logger.Log(Sender, LogType, Description);
  AddToReferenceLists(Sender, LogType, Description, IDs);
  Sleep(25);  // To make sure info is written to file before comparing it.
end{procedure};

procedure TLogTest.StartCompare;
begin
  FreeAndNil(Logger); {Releases the log files for opening}
end{procedure};

procedure TLogTest.VerifyNumberOfLines(ID, ExpectedLines: Integer);
var
  FC: TStrings;
begin
  FC := TStringList.Create;
  try
    FC.LoadFromFile(ReferenceLogFiles[ID]);
    TestCase.CheckEquals(ExpectedLines, FC.Count);
  finally
    FC.Free;
  end{finally};
end{procedure};

procedure TLogTest.CompareLine(ID, LineNo: Integer; const Expected: String);
var
  FC: TStrings;
begin
  FC := TStringList.Create;
  try
    FC.LoadFromFile(ReferenceLogFiles[ID]);
    CompareLineInternal(Expected, FC[LineNo]);
  finally
    FC.Free;
  end{finally};
end{procedure};

procedure TLogTest.Compare(ID, NoOfLines: Integer);
var
  i: Integer;
  FC: TStrings;
begin
  FC := TStringList.Create;
  try
    VerifyNumberOfLines(ID, NoOfLines);
    FC.LoadFromFile(ReferenceLogFiles[ID]);
    for i := 0 to NoOfLines-1 do
      CompareLineInternal((ReferenceLists[ID] as TStringList)[i], FC[i]);
  finally
    FC.Free;
  end{finally};
end{procedure};

procedure TLogTest.Stop;
var
  i: Integer;
begin
  for i := 0 to ReferenceLogFiles.Count-1 do
    DeleteFile(ReferenceLogFiles[i]);
  ReferenceLogFiles.Clear;
  ReferenceLists.Clear;
end{procedure};

constructor TLogTest.Create(aTestCase: TTestCase);
begin
  TestCase := aTestCase;
  ReferenceLists := TObjectList.Create;
  ReferenceLogFiles := TStringList.Create;
end{constructor};

destructor TLogTest.Destroy;
begin
  Stop;
  ReferenceLogFiles.Free;
  ReferenceLists.Free;
  inherited;
end{destructor};

end.
