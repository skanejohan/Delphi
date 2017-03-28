program adsTokenizerTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  adsTokenizerClass,
  adsTokenizerTestCases in 'adsTokenizerTestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
