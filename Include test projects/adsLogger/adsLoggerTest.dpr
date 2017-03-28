program adsLoggerTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  adsLoggerTestCases in 'adsLoggerTestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
