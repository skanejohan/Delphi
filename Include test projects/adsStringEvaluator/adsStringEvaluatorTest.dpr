program adsStringEvaluatorTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  adsStringEvaluatorClass,
  adsStringEvaluatorTestCases,
  adsStringMethods;

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
