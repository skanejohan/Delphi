program adsExecutorClassTest;

uses
  Forms,
  adsExecutorClassTestFrm in 'adsExecutorClassTestFrm.pas' {adsExecutorClassTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TadsExecutorClassTestForm, adsExecutorClassTestForm);
  Application.Run;
end.
