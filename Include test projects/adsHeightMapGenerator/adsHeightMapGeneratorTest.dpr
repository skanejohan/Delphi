program adsHeightMapGeneratorTest;

uses
  Forms,
  adsHeightMapGeneratorTestFrm in 'adsHeightMapGeneratorTestFrm.pas' {adsHeightMapGeneratorTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TadsHeightMapGeneratorTestForm, adsHeightMapGeneratorTestForm);
  Application.Run;
end.
