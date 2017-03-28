program TextEditDialogTest;

uses
  Forms,
  adsTextEditDlg,
  TextEditDialogTestFrm in 'TextEditDialogTestFrm.pas' {TextEditDialogTestForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTextEditDialogTestForm, TextEditDialogTestForm);
  Application.CreateForm(TadsTextEditDialog, adsTextEditDialog);
  Application.Run;
end.
