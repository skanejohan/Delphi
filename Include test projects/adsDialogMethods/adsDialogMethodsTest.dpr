program adsDialogMethodsTest;

uses
  Forms,
  adsDialogMethodsTestFrm in 'adsDialogMethodsTestFrm.pas' {adsDialogMethodsTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TadsDialogMethodsTestForm, adsDialogMethodsTestForm);
  Application.Run;
end.
