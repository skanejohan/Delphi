unit TextEditDialogTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TTextEditDialogTestForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure TextEditDialogValidateText(Sender: TObject; Text: String;
      var OK: Boolean);
  public
    { Public declarations }
  end;

var
  TextEditDialogTestForm: TTextEditDialogTestForm;

implementation

{$R *.DFM}

uses
  adsTextEditDlg, adsDialogMethods;

procedure TTextEditDialogTestForm.Button1Click(Sender: TObject);
begin
  with adsTextEditDialog do
  begin
    OnValidateText := TextEditDialogValidateText;
    Text := Edit1.Text;
    Caption := 'Enter password';
    AllowEmptyText := CheckBox1.Checked;
    if ShowModal = mrOK then
      Edit1.Text := Text;
  end;
end;

procedure TTextEditDialogTestForm.TextEditDialogValidateText(Sender: TObject;
  Text: String; var OK: Boolean);
begin
  OK := ConfirmationDialog(Self, Text + ' OK?') = mrYes;
end; 

end.
