program _adsThreadedDrawingControlTest;

uses
  Forms,
  _adsThreadedDrawingControlTestFrm in '_adsThreadedDrawingControlTestFrm.pas' {adsThreadedDrawingControlTestForm},
  adsThreadedDrawingControlClass in '..\..\..\Include\Delphi\adsThreadedDrawingControlClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TadsThreadedDrawingControlTestForm, adsThreadedDrawingControlTestForm);
  Application.Run;
end.
