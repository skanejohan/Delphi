program adsGeometryUnitTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  adsGeometryUnitTestCases in 'adsGeometryUnitTestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
