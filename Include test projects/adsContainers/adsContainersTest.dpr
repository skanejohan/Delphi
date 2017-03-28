program adsContainersTest;

uses
  Forms,
  adsContainersTestFrm in 'adsContainersTestFrm.pas' {adsContainersTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TadsContainersTestForm, adsContainersTestForm);
  Application.Run;
end.
