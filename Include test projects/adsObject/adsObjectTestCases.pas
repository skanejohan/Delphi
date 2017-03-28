unit adsObjectTestCases;

interface

uses
  TestFrameWork, adsObjectClass, LogTestClass;

type
  TadsObjectTestCase = class(TTestCase)
  protected
    DataDir: String;
    LogTest: TLogTest;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SetupAndTakeDownOK;
    procedure SetupAndTakeDownMemLeak;
    procedure SetupAndTakeDownOKWithLogCreateAndDestroy;
    procedure SetupAndTakeDownMemLeakWithLogCreateAndDestroy;
  end{class};

implementation

uses
  Classes, Forms, SysUtils, adsFileMethods, adsLoggerClass, adsObjectTestClasses;

procedure TadsObjectTestCase.SetUp;
begin
  inherited;
  DataDir := ExtractFilePath(Application.ExeName) + '\Data\';
  LogTest := TLogTest.Create(Self);
  ForceDirectories(DataDir);
end{procedure};

procedure TadsObjectTestCase.TearDown;
begin
  inherited;
  LogTest.Free;
  Sleep(25);
  DeleteDirectory(DataDir);
end{procedure};

procedure TadsObjectTestCase.SetupAndTakeDownOK;
var
  LogID: Integer;
begin
  LogCreateAndDestroy := False;
  LogTest.StartLogging;
  LogID := LogTest.AddSubscription(DataDir + 'SetupAndTakeDownOK.log', [ltMemDebug, ltMemError]);
  with TadsObject1.Create do
    Free;
  with TadsObject2.Create do
    Free;
  //ClearObjects;
  LogTest.StartCompare;
  LogTest.Compare(LogID, 0);
  LogTest.Stop;
end{procedure};

procedure TadsObjectTestCase.SetupAndTakeDownMemLeak;
var
  LogID: Integer;
  O1: TadsObject1;
  O2: TadsObject2;
begin
  LogCreateAndDestroy := False;
  LogTest.StartLogging;
  LogID := LogTest.AddSubscription(DataDir + 'SetupAndTakeDownMemLeak.log', [ltMemDebug, ltMemError]);
  O1 := TadsObject1.Create;
  LogTest.AddToReferenceLists(nil, ltMemError, 'Memory leak: TadsObject1 - "TADSOBJECT1"', [LogID]);
  O2 := TadsObject2.Create;
  LogTest.AddToReferenceLists(nil, ltMemError, 'Memory leak: TadsObject2', [LogID]);
  //ClearObjects;
  O1.Free;
  O2.Free;
  LogTest.StartCompare;
  LogTest.Compare(LogID, 2);
  LogTest.Stop;
end{procedure};

procedure TadsObjectTestCase.SetupAndTakeDownOKWithLogCreateAndDestroy;
var
  LogID: Integer;
  O1: TadsObject1;
  O2: TadsObject2;
begin
  LogCreateAndDestroy := True;
  LogTest.StartLogging;
  LogID := LogTest.AddSubscription(DataDir + 'SetupAndTakeDownOKWithLogCreateAndDestroy.log', [ltMemDebug, ltMemError]);
  O1 := TadsObject1.Create;
  LogTest.AddToReferenceLists(O1, ltMemDebug, 'Create: TADSOBJECT1', [LogID]);
  LogTest.AddToReferenceLists(O1, ltMemDebug, 'Destroy: TADSOBJECT1', [LogID]);
  O1.Free;
  O2 := TadsObject2.Create;
  LogTest.AddToReferenceLists(O2, ltMemDebug, 'Create: (TadsObject2)', [LogID]);
  LogTest.AddToReferenceLists(O2, ltMemDebug, 'Destroy: (TadsObject2)', [LogID]);
  O2.Free;
  //ClearObjects;
  LogTest.StartCompare;
  LogTest.Compare(LogID, 4);
  LogTest.Stop;
end{procedure};

procedure TadsObjectTestCase.SetupAndTakeDownMemLeakWithLogCreateAndDestroy;
var
  LogID: Integer;
  O1: TadsObject1;
  O2: TadsObject2;
begin
  LogCreateAndDestroy := True;
  LogTest.StartLogging;
  LogID := LogTest.AddSubscription(DataDir + 'SetupAndTakeDownMemLeakWithLogCreateAndDestroy.log', [ltMemDebug, ltMemError]);
  O1 := TadsObject1.Create;
  LogTest.AddToReferenceLists(O1, ltMemDebug, 'Create: TADSOBJECT1', [LogID]);
  O2 := TadsObject2.Create;
  LogTest.AddToReferenceLists(O2, ltMemDebug, 'Create: (TadsObject2)', [LogID]);
  LogTest.AddToReferenceLists(nil, ltMemError, 'Memory leak: TadsObject1 - "TADSOBJECT1"', [LogID]);
  LogTest.AddToReferenceLists(nil, ltMemError, 'Memory leak: TadsObject2', [LogID]);
  //ClearObjects;
  O1.Free;
  O2.Free;
  LogTest.StartCompare;
  LogTest.Compare(LogID, 4);
  LogTest.Stop;
end{procedure};

initialization
  TestFramework.RegisterTest(TadsObjectTestCase.Suite);

end.
