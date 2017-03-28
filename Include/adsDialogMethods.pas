unit adsDialogMethods;

{ © 2002-2010 Aston Design Studio }

interface

uses
  Classes, Forms;

{** Displays a "ShowMessage" dialog listing all items in Messages. }
procedure ShowMessages(Messages: TStrings);

{** Displays an information dialog. }
function InformationDialog(CreatingForm: TForm; const Msg: String): Word;

{** Displays a confirmation dialog. }
function ConfirmationDialog(CreatingForm: TForm; const Msg: String;
  AllowCancel: Boolean=False): Word;

{** Shows a modal dialog, centered around the creating form. }
function ShowModalCentered(Dialog, CreatingForm: TForm): Word;

implementation

uses
  Controls, StdCtrls, Dialogs;

const
  DialogCaptions: array[TMsgDlgType] of string[20] = ('Warning', 'Error',
    'Information', 'Confirmation', '');
  ButtonCaptions: array[TMsgDlgBtn] of string[12] = ('Yes', 'No', 'OK',
    'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'No to all','Yes to all',
    'Help');

procedure ShowMessages(Messages: TStrings);
var
  i: Integer;
  Str: String;
begin
  Str := '';
  for i := 0 to Messages.Count-1 do
    Str := Str + Messages[i] + #13 + #10;
  if Str <> '' then
    Str := Copy(Str, 1, Length(Str)-2);
  ShowMessage(Str);
end{procedure};

procedure CenterDialog(Dialog, CreatingForm: TForm);
begin
  Dialog.Top := CreatingForm.Top + (CreatingForm.Height - Dialog.Height) div 2;
  Dialog.Left := CreatingForm.Left + (CreatingForm.Width - Dialog.Width) div 2;
  if Dialog.Top < 0 then
    Dialog.Top := 0
  else if (Dialog.Top + Dialog.Height) > Screen.Height then
    Dialog.Top := Screen.Height - Dialog.Height;
  if Dialog.Left < 0 then
    Dialog.Left := 0
  else if (Dialog.Left + Dialog.Width) > Screen.Width then
    Dialog.Left := Screen.Width - Dialog.Width;
end{procedure};

function ModifiedDialog(CreatingForm: TForm; X, Y: Integer; const Msg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Word;
var
  Dialog: TForm;
  i: Integer;
  ButtonType: TMsgDlgBtn;
  OldCursor: TCursor;

  procedure ChangeCaption;
  begin
    try
      repeat
        Inc(i)
      until Dialog.Components[i] is TButton;
      with Dialog.Components[i] as TButton do
        Caption := ButtonCaptions[ButtonType];
    except
      {do nothing}
    end{except};
  end{procedure};

begin
  OldCursor := Screen.Cursor;
  Dialog := CreateMessageDialog(Msg, DlgType, Buttons);
  with Dialog do
    try
      Caption := DialogCaptions[DlgType];
      i := -1;
      for ButtonType := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
        if ButtonType in Buttons then ChangeCaption;
      HelpContext := HelpCtx;
      if CreatingForm <> nil then
        CenterDialog(Dialog, CreatingForm)
      else
      begin
        if X >= 0 then
          Left := X
        else
          Left := Screen.Width div 2 - Width div 2;
        if Y >= 0 then
          Top := Y
        else
          Top := Screen.Height div 2 - Height div 2;
      end{else};
      Screen.Cursor := crDefault;
      Result := ShowModal;
      if CreatingForm <> nil then
        CreatingForm.Update;
    finally
      Screen.Cursor := OldCursor;
      Free;
    end{finally};
end{function};

function InformationDialog(CreatingForm: TForm; const Msg: string): Word;
begin
  Result := ModifiedDialog(CreatingForm, 0, 0, Msg, mtInformation, [mbOk], 0);
end{function};

function ConfirmationDialog(CreatingForm: TForm; const Msg: string; AllowCancel:
  Boolean=False): Word;
begin
  if AllowCancel then
    Result := ModifiedDialog(CreatingForm, 0, 0, Msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0)
  else
    Result := ModifiedDialog(CreatingForm, 0, 0, Msg, mtConfirmation, [mbYes, mbNo], 0);
end{function};

function ShowModalCentered(Dialog, CreatingForm: TForm): Word;
var
  OldCursor: TCursor;
begin
  CenterDialog(Dialog, CreatingForm);
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crDefault;
    Result := Dialog.ShowModal;
    if CreatingForm <> nil then CreatingForm.Update;
  finally
    Screen.Cursor := OldCursor;
  end{finally};
end{function};

end.
