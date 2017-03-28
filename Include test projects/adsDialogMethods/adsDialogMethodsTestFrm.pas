unit adsDialogMethodsTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TadsDialogMethodsTestForm = class(TForm)
    ShowMessagesButton: TButton;
    InformationDialogButton: TButton;
    ConfirmationDialogButton: TButton;
    procedure ShowMessagesButtonClick(Sender: TObject);
    procedure InformationDialogButtonClick(Sender: TObject);
    procedure ConfirmationDialogButtonClick(Sender: TObject);
  end{class};

var
  adsDialogMethodsTestForm: TadsDialogMethodsTestForm;

implementation

{$R *.dfm}

uses
  adsDialogMethods;

procedure TadsDialogMethodsTestForm.ShowMessagesButtonClick(Sender: TObject);
var
  Messages: TStrings;
begin
  Messages := TStringList.Create;
  try
    Messages.Add('This is message 1');
    Messages.Add('This is message 2');
    Messages.Add('');
    Messages.Add('This is message 3');
    ShowMessages(Messages);
  finally
    Messages.Free;
  end{finally};
end{procedure};

procedure TadsDialogMethodsTestForm.InformationDialogButtonClick(Sender: TObject);
begin
  InformationDialog(Self, 'This is a piece of information');
end{procedure};

procedure TadsDialogMethodsTestForm.ConfirmationDialogButtonClick(Sender: TObject);
  procedure Display(Value: Integer);
  begin
    case Value of
      mrYes: ShowMessage('Yes');
      mrNo: ShowMessage('No');
      mrCancel: ShowMessage('Cancel');
    else
      ShowMessage(IntToStr(Value));
    end{case};
  end{procedure};
begin
  Display(ConfirmationDialog(Self, 'Please confirm'));
  Display(ConfirmationDialog(Self, 'Please confirm again', True));
end{procedure};

end.
