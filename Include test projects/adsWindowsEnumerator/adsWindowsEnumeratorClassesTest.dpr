program adsWindowsEnumeratorClassesTest;

uses
  Forms,
  adsWindowsEnumeratorClassesTestFrm in 'adsWindowsEnumeratorClassesTestFrm.pas' {adsWindowsEnumeratorClassesTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TadsWindowsEnumeratorClassesTestForm, adsWindowsEnumeratorClassesTestForm);
  Application.Run;
end.
