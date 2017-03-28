program _adsMapControlTest;

uses
  Forms,
  _adsMapControlTestFrm in '_adsMapControlTestFrm.pas' {adsMapControlTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TadsMapControlTestForm, adsMapControlTestForm);
  Application.Run;
end.
