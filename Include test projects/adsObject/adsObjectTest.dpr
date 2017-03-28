program adsObjectTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  adsObjectTestCases in 'adsObjectTestCases.pas',
  adsObjectTestClasses in 'adsObjectTestClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
