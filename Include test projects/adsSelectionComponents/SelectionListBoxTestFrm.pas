unit SelectionListBoxTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  adsSelectionListBoxComp, ExtCtrls, StdCtrls, adsSelectionTreeViewComp,
  adsSelectionCompTypes;

type
  TForm1 = class(TForm)
    SelectedEdit: TEdit;
    Label1: TLabel;
    WriteSelectedButton: TButton;
    CheckBox1: TCheckBox;
    FirstItemEdit: TEdit;
    Label3: TLabel;
    SetFirstItemButton: TButton;
    ClearFirstItemCheckBox: TCheckBox;
    ClearEditCheckBox: TCheckBox;
    ClearButton: TButton;
    AddItemsButton: TButton;
    RadioGroup1: TRadioGroup;
    Panel2: TPanel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    CheckBox4: TCheckBox;
    Button3: TButton;
    Button4: TButton;
    RadioGroup2: TRadioGroup;
    Memo1: TMemo;
    GetListItemsButton: TButton;
    Memo2: TMemo;
    GetTreeItemsButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure AddItemsButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure WriteSelectedButtonClick(Sender: TObject);
    procedure SetFirstItemButtonClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure GetListItemsButtonClick(Sender: TObject);
    procedure GetTreeItemsButtonClick(Sender: TObject);
    procedure SelectionListBoxSelectionChanged(Sender: TadsSelectionListBox;
      Item: String);
    procedure SelectionTreeViewSelectionChanged(Sender: TadsSelectionTreeView;
      Item: String);
  protected
    SelectionListBox: TadsSelectionListBox;
    SelectionTreeView: TadsSelectionTreeView;
    procedure AddItems;
    procedure AddTreeViewItems;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ Code related to the SelectionListBox }

procedure TForm1.AddItems;
begin
  with SelectionListBox do
  begin
    AddItem('Sweden');
    AddItem('Denmark');
    AddItem('Norway');
    AddItem('Finland');
    AddItem('Iceland');
    AddItem('Germany');
    AddItem('France');
    AddItem('Italy');
    AddItem('Albania');
    AddItem('All countries');
  end;
end;

procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  SelectionListBox.ClearItems(ClearFirstItemCheckBox.Checked,
    ClearEditCheckBox.Checked);
end;

procedure TForm1.AddItemsButtonClick(Sender: TObject);
begin
  AddItems;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  SelectionListBox.Sorted := CheckBox1.Checked;
end;

procedure TForm1.SelectionListBoxSelectionChanged(
  Sender: TadsSelectionListBox; Item: String);
begin
  SelectedEdit.Text := Item;
end;

procedure TForm1.WriteSelectedButtonClick(Sender: TObject);
begin
  SelectionListBox.Item := SelectedEdit.Text;
end;

procedure TForm1.SetFirstItemButtonClick(Sender: TObject);
begin
  SelectionListBox.FirstItem := FirstItemEdit.Text;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  SelectionListBox.EditMode := TEditMode(RadioGroup1.ItemIndex);
end;

procedure TForm1.GetListItemsButtonClick(Sender: TObject);
begin
  SelectionListBox.GetItemsAsStrings(Memo1.Lines);
end;

{ Code related to the SelectionTreeView }

procedure TForm1.AddTreeViewItems;
begin
  with SelectionTreeView do
  begin
    AddItem('Europe\Scandinavia\Sweden');
    AddItem('Europe\Scandinavia\Denmark');
    AddItem('Europe\Scandinavia\Norway');
    AddItem('Europe\Scandinavia\Finland');
    AddItem('Europe\Scandinavia\Iceland');
    AddItem('Europe\Continent\Germany');
    AddItem('Europe\Mediteranean\France');
    AddItem('Europe\Mediteranean\Italy');
    AddItem('Europe\Mediteranean\Albania');
    AddItem('America\USA');
    AddItem('America\Canada');
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SelectionTreeView.ClearItems(CheckBox4.Checked);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  AddTreeViewItems;
end;

procedure TForm1.SelectionTreeViewSelectionChanged(
  Sender: TadsSelectionTreeView; Item: String);
begin
  Edit1.Text := Item;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SelectionTreeView.Item := Edit1.Text;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  SelectionTreeView.EditMode := TEditMode(RadioGroup2.ItemIndex);
end;

procedure TForm1.GetTreeItemsButtonClick(Sender: TObject);
begin
  SelectionTreeView.GetItemsAsStrings(Memo2.Lines);
end;

{ Constructor }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SelectionListBox := TadsSelectionListBox.Create(Self);
  with SelectionListBox do
  begin
    Parent := Self;
    Left := 264;
    Top := 8;
    Width := 185;
    Height := 289;
    Caption := 'SelectionListBox';
    TabOrder := 21;
    EditMode := emStatic;
    Sorted := True;
    OnSelectionChanged := SelectionListBoxSelectionChanged;
  end; {with}
  SelectionTreeView := TadsSelectionTreeView.Create(Self);
  with SelectionTreeView do
  begin
    Parent := Self;
    Left := 264;
    Top := 336;
    Width := 185;
    Height := 257;
    Caption := 'SelectionTreeView';
    TabOrder := 22;
    EditMode := emStatic;
    OnSelectionChanged := SelectionTreeViewSelectionChanged;
  end; {with}
  SelectionListBox.FirstItem := 'All countries';
  AddItems;
  AddTreeViewItems;
end;

end.
