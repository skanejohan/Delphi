unit OrderedStringListFormTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  adsOrderedStringsFrm, StdCtrls, FileCtrl;

type
  TOrderedStringListFormTestForm = class(TForm)
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure FileListBox1DblClick(Sender: TObject);
  end{class};

var
  OrderedStringListFormTestForm: TOrderedStringListFormTestForm;

implementation

{$R *.DFM}

procedure TOrderedStringListFormTestForm.FormShow(Sender: TObject);
begin
  adsOrderedStringsForm.Show;
end;

procedure TOrderedStringListFormTestForm.FileListBox1DblClick(
  Sender: TObject);
begin
  adsOrderedStringsForm.AddString(DirectoryListBox1.Directory + '\' +
    FileListBox1.Items[FileListBox1.ItemIndex]);
end;

end.
