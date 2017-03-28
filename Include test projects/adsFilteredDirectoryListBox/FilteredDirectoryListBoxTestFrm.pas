unit FilteredDirectoryListBoxTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, adsFilteredDirectoryListBoxComp, FileCtrl;

type
  TForm1 = class(TForm)
    DirectoryListBox1: TDirectoryListBox;
    Button1: TButton;
    Edit1: TEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button2: TButton;
    Button6: TButton;
    AllowedDirectoriesMemo: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FilteredDirectoryListBoxChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FilteredDirectoryListBox: TadsFilteredDirectoryListBox;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    FilteredDirectoryListBox.AddDirectory(DirectoryListBox1.Directory, True);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FilteredDirectoryListBox.AddDirectory(DirectoryListBox1.Directory, False);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  with FilteredDirectoryListBox do
    RemoveDirectory(Directory);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FilteredDirectoryListBox.Directory := Edit1.Text;
end;

procedure TForm1.FilteredDirectoryListBoxChange(Sender: TObject);
begin
  Edit1.Text := FilteredDirectoryListBox.Directory;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AllowedDirectoriesMemo.Lines.Assign(FilteredDirectoryListBox.AllowedDirectories);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FilteredDirectoryListBox.AllowedDirectories.Assign(AllowedDirectoriesMemo.Lines);
  FilteredDirectoryListBox.UpdateDirectories;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FilteredDirectoryListBox := TadsFilteredDirectoryListBox.Create(Self);
  with FilteredDirectoryListBox do
  begin
    Left := 256;
    Top := 8;
    Width := 209;
    Height := 337;
    Parent := Self;
    OnChange := FilteredDirectoryListBoxChange;
  end; {with}
end;

end.
