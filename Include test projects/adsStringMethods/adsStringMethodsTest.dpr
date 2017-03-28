program adsStringMethodsTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  adsStringMethodsTestCases in 'adsStringMethodsTestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
