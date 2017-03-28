program AStarTest;

uses
  Forms,
  AStarTestFrm in 'AStarTestFrm.pas' {AStarTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAStarTestForm, AStarTestForm);
  Application.Run;
end.
