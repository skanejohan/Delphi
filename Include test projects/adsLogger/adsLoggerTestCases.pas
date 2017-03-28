unit adsLoggerTestCases;

interface

uses
  Classes, TestFrameWork, adsLoggerClass, LogTestClass;

type
  TadsLoggerTestCase = class(TTestCase)
  protected
    LogTest: TLogTest;
    procedure SetUp; override;
    procedure TearDown; override; 
    function DataDir: String;
  published
    procedure TestOneFileAllLogTypes;
    procedure TestOneFilePerType;
    procedure TestMaxFileSizeWithoutBackup;
    procedure TestMaxFileSizeWithBackup;
  end{class};

implementation

uses
  Windows, Forms, SysUtils, adsFileMethods;

function TadsLoggerTestCase.DataDir: String;
begin
  Result := ExtractFilePath(Application.ExeName) + '\Data\';
end{function};

procedure TadsLoggerTestCase.SetUp;
begin
  inherited;
  LogTest := TLogTest.Create(Self);
  ForceDirectories(DataDir);
end{procedure};

procedure TadsLoggerTestCase.TearDown;
begin
  inherited;
  LogTest.Free;
  DeleteDirectory(DataDir);
end{procedure};

procedure TadsLoggerTestCase.TestOneFileAllLogTypes;
var
  LogID: Integer;
begin
  LogTest.StartLogging;
  LogID := LogTest.AddSubscription(DataDir + 'one.log',
    [ltError, ltWarning, ltInformation, ltDebug]);
  LogTest.Log(Self, ltError, 'Wrong!', [LogID]);
  LogTest.Log(Self, ltWarning, 'Odd!', [LogID]);
  LogTest.Log(Self, ltInformation, 'Weird, but alright', [LogID]);
  LogTest.Log(Self, ltDebug, 'strange, this might be the cause', [LogID]);
  LogTest.StartCompare;
  LogTest.Compare(LogID, 4);
  LogTest.Stop;

  // Now verify correct behaviour of TLogTest (AddToReferenceLists)
  LogTest.StartLogging;
  LogID := LogTest.AddSubscription(DataDir + 'one.log',
    [ltError, ltWarning, ltInformation, ltDebug]);
  LogTest.Log(Self, ltError, 'Wrong!', []);
  LogTest.AddToReferenceLists(Self, ltError, 'Wrong!', [LogID]);
  LogTest.Log(Self, ltWarning, 'Odd!', []);
  LogTest.AddToReferenceLists(Self, ltWarning, 'Odd!', [LogID]);
  LogTest.Log(Self, ltInformation, 'Weird, but alright', []);
  LogTest.AddToReferenceLists(Self, ltInformation, 'Weird, but alright', [LogID]);
  LogTest.Log(Self, ltDebug, 'strange, this might be the cause', []);
  LogTest.AddToReferenceLists(Self, ltDebug, 'strange, this might be the cause', [LogID]);
  LogTest.StartCompare;
  LogTest.Compare(LogID, 4);
  LogTest.Stop;
end{procedure};

procedure TadsLoggerTestCase.TestOneFilePerType;
var
  ErrorLogID, WarningLogID, InformationLogID, DebugLogID: Integer;
begin
  LogTest.StartLogging;
  ErrorLogID := LogTest.AddSubscription(DataDir + 'Error.log', [ltError]);
  WarningLogID := LogTest.AddSubscription(DataDir + 'Warning.log', [ltWarning]);
  InformationLogID := LogTest.AddSubscription(DataDir + 'Information.log', [ltInformation]);
  DebugLogID := LogTest.AddSubscription(DataDir + 'Debug.log', [ltDebug]);
  LogTest.Log(Self, ltError, 'Wrong!', [ErrorLogID]);
  LogTest.Log(Self, ltWarning, 'Odd!', [WarningLogID]);
  LogTest.Log(Self, ltInformation, 'Weird, but alright', [InformationLogID]);
  LogTest.Log(Self, ltDebug, 'strange, this might be the cause', [DebugLogID]);
  LogTest.StartCompare;
  LogTest.Compare(ErrorLogID, 1);
  LogTest.Compare(WarningLogID, 1);
  LogTest.Compare(InformationLogID, 1);
  LogTest.Compare(DebugLogID, 1);
  LogTest.Stop;
end{procedure};

procedure TadsLoggerTestCase.TestMaxFileSizeWithoutBackup;
var
  Files: TStrings;
  FileName: String;
  i, LogID: Integer;
begin
  FileName := DataDir + 'test.log';
  DeleteFile(FileName);
  Files := TStringList.Create;
  try
    LogTest.StartLogging;
    LogID := LogTest.AddSubscription(FileName, [ltInformation], 2, False);
    for i := 1 to 18 do
      LogTest.Log(Self, ltInformation, 'This text contributes 116 bytes towards the total file size', []);
    // Now, the log file will be restarted - add a new log and compare against it.
    LogTest.Log(Self, ltInformation, 'This text contributes 116 bytes towards the total file size', []);
    LogTest.AddToReferenceLists(Self, ltInformation, 'This text contributes 116 bytes towards the total file size', [LogID]);
    LogTest.StartCompare;
    LogTest.Compare(LogID, 1);
    GetFiles(DataDir, Files);
    Check(Files.Count = 1, 'Invalid number of files: ' + IntToStr(Files.Count));
  finally
    Files.Free;
  end{finally};
end{procedure};

procedure TadsLoggerTestCase.TestMaxFileSizeWithBackup;
var
  Files: TStrings;
  FileName: String;
  i, LogID: Integer;
begin
  FileName := DataDir + 'test.log';
  DeleteFile(FileName);
  Files := TStringList.Create;
  try
    LogTest.StartLogging;
    LogID := LogTest.AddSubscription(FileName, [ltInformation], 2, True);
    for i := 1 to 18 do
      LogTest.Log(Self, ltInformation, 'This text contributes 116 bytes towards the total file size', []);
    // Now, the log file will be restarted - add a new log and compare against it.
    LogTest.Log(Self, ltInformation, 'This text contributes 116 bytes towards the total file size', []);
    LogTest.AddToReferenceLists(Self, ltInformation, 'This text contributes 116 bytes towards the total file size', [LogID]);
    LogTest.StartCompare;
    LogTest.Compare(LogID, 1);
    GetFiles(DataDir, Files);
    Check(Files.Count = 2, 'Invalid number of files: ' + IntToStr(Files.Count));
  finally
    Files.Free;
  end{finally};
end{procedure};

initialization
  TestFramework.RegisterTest(TadsLoggerTestCase.Suite);

end.
