unit adsTextEditDlg;

{ © 2002-2010 Aston Design Studio }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Spin;

type
  TValidateTextEvent = procedure(Sender: TObject; Text: String;
    var OK: Boolean) of object;

  {** This dialog allows the user to edit a text. }
  TadsTextEditDialog = class(TForm)
    CancelBtn: TBitBtn;
    OKBtn: TBitBtn;
    TextEdit: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    FAllowEmptyText: Boolean;
    FOnValidateText: TValidateTextEvent;
    function GetText: String;
    procedure SetText(Value: String);
  public

    {** Set this property to the text that should be initially displayed in the
     edit box when the dialog is displayed. Read this property after the dialog
     is closed to get the text that was entered. }
    property Text: String read GetText write SetText;

    {** Set this property to True if the user is allowed to leave the text edit
     box empty, otherwise to False. If False, a message will be displayed to the
     user if OK is clicked when the edit box is empty. }
    property AllowEmptyText: Boolean read FAllowEmptyText write FAllowEmptyText;

    {** This event is generated when the user clicks OK. The event handler fills
     in the OK parameter and possibly shows a message. If OK is set to False,
     the edit box receives focus and the dialog is not closed. }
    property OnValidateText: TValidateTextEvent read FOnValidateText
      write FOnValidateText;
  end{class};

var
  adsTextEditDialog: TadsTextEditDialog;

implementation

uses
  adsDialogMethods;

{$R *.DFM}

function TadsTextEditDialog.GetText: String;
begin
  Result := TextEdit.Text;
end{function};

procedure TadsTextEditDialog.SetText(Value: String);
begin
  TextEdit.Text := Value;
end{procedure};

procedure TadsTextEditDialog.FormShow(Sender: TObject);
begin
  TextEdit.SetFocus;
  TextEdit.SelectAll;
end{procedure};

procedure TadsTextEditDialog.OKBtnClick(Sender: TObject);
var
  OK: Boolean;
begin
  OK := True;
  if (TextEdit.Text = '') and (not FAllowEmptyText) then
  begin
    InformationDialog(Self, 'This field must not be empty!');
    OK := False;
  end{if}
  else if Assigned(OnValidateText) then
    OnValidateText(Self, Text, OK);
  if not OK then
  begin
    TextEdit.SetFocus;
    TextEdit.SelectAll;
    ModalResult := mrNone;
  end{if};
end{procedure};

end.
