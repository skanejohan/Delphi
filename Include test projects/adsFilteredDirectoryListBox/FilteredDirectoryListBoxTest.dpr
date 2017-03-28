program FilteredDirectoryListBoxTest;

uses
  Forms,
  FilteredDirectoryListBoxTestFrm in 'FilteredDirectoryListBoxTestFrm.pas' {Form1},
  FileCtrl in 'filectrl.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
