program adsFileMethodsTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  adsFileMethodsTestCases in 'adsFileMethodsTestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
