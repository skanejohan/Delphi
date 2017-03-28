unit SelectBoxTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  adsSelectBoxComp, adsDBSelectBoxComp, ExtCtrls, StdCtrls, CheckLst, DBCtrls,
  Db, DBTables, Grids, DBGrids, Mask, adsIntegratedDBSelectBoxComp;

type
  TStringArray = Array of string;

  TForm1 = class(TForm)
    SelectBoxPanel: TPanel;
    DBSelectBoxPanel: TPanel;
    ListBox1: TListBox;
    CategoryTable: TTable;
    ElementTable: TTable;
    ElementCategoryTable: TTable;
    ListBox2: TListBox;
    CategoryDS: TDataSource;
    ElementDS: TDataSource;
    CategoryElementDS: TDataSource;
    Edit1: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure SelectBoxSelectionChanged(Sender: TObject; Element, Category: String;
      ItemSelected: Boolean);
    procedure Button1Click(Sender: TObject);
  protected
    SelectBox: TadsSelectBox;
    DBSelectBox: TadsDBSelectBox;
    IntegratedDBSelectBox: TadsIntegratedDBSelectBox;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  adsStringMethods;

{ Select box }

procedure TForm1.ListBox1Click(Sender: TObject);
var
  i: Integer;
  Item: String;
begin
  SelectBox.ClearSelection;
  for i := 0 to ListBox1.Items.Count-1 do
    if ListBox1.Selected[i] then
      SelectBox.SelectFor(Item, RightPart(ListBox1.Items[i], ';'));
end;

procedure TForm1.SelectBoxSelectionChanged(Sender: TObject; Element, Category: String;
  ItemSelected: Boolean);
var
  i: Integer;
  Str: String;
begin
  with ListBox1 do
    for i := 0 to Items.Count-1 do
      if LeftPart(Items[i], ';') = Element then
      begin
        if ItemSelected then
          Items[i] := Items[i] + ';' + Category
        else
        begin
          Str := Items[i];
          Delete(Str, Pos(';'+Category, Str), Length(Category)+1);
          Items[i] := Str;
        end;
      end;
end;

{ DbSelectBox }

procedure TForm1.ListBox2Click(Sender: TObject);
var
  i: Integer;
begin
  DBSelectBox.ClearSelection;
  for i := 0 to ListBox2.Items.Count-1 do
    if ListBox2.Selected[i] then
      DBSelectBox.SelectFor(ListBox2.Items[i]);
end;

{ Create }

procedure TForm1.FormCreate(Sender: TObject);
var
  Categories: TStrings;
begin
  Categories := TStringList.Create;
  try
    SelectBox := TadsSelectBox.Create(Self);
    with SelectBox do
    begin
      Parent := SelectBoxPanel;
      Left := 1;
      Top := 1;
      Width := 199;
      Height := 175;
      Align := alClient;
      ItemHeight := 13;
      TabOrder := 0;
      OnSelectionChanged := SelectBoxSelectionChanged;
    end; {with}
    DBSelectBox := TadsDBSelectBox.Create(Self);
    with DBSelectBox do
    begin
      Parent := DBSelectBoxPanel;
      Left := 1;
      Top := 1;
      Width := 183;
      Height := 175;
      Align := alClient;
      ItemHeight := 13;
      TabOrder := 0;
      CategoryDataSource := CategoryDS;
      CategoryDataSourceIDField := 'ID';
      CategoryDataSourceNameField := 'Name';
      ElementDataSource := ElementDS;
      ElementDataSourceIDField := 'ID';
      ElementDataSourceNameField := 'Name';
      CategoryElementDataSource := CategoryElementDS;
      CategoryElementDataSourceCategoryIDField := 'CategoryID';
      CategoryElementDataSourceElementIDField := 'ElementID';
    end; {with}
    IntegratedDBSelectBox := TadsIntegratedDBSelectBox.Create(Self);
    with IntegratedDBSelectBox do
    begin
      Parent := Self;
      Left := 408;
      Top := 0;
      Width := 185;
      Height := 401;
      Caption := 'IntegratedDBSelectBox';
      TabOrder := 4;
      CategoryDataSource := CategoryDS;
      CategoryDataSourceIDField := 'ID';
      CategoryDataSourceNameField := 'Name';
      ElementDataSource := ElementDS;
      ElementDataSourceIDField := 'ID';
      ElementDataSourceNameField := 'Name';
      CategoryElementDataSource := CategoryElementDS;
      CategoryElementDataSourceCategoryIDField := 'CategoryID';
      CategoryElementDataSourceElementIDField := 'ElementID';
      ListBoxHeight := 220;
    end; {with}

    CategoryTable.DatabaseName := GetCurrentDir + '\Data';
    ElementTable.DatabaseName := GetCurrentDir + '\Data';
    ElementCategoryTable.DatabaseName := GetCurrentDir + '\Data';

    { SelectBox }
    Categories.Add('Blue');
    Categories.Add('Red');
    Categories.Add('Yellow');
    Categories.Add('White');
    Categories.Add('Green');
    Categories.Add('Black');
    SelectBox.Initialise(Categories);

    { DbSelectBox }
    CategoryTable.Open;
    ElementCategoryTable.Open;
    with ElementTable do
    begin
      Open;
      while not Eof do
      begin
        ListBox2.Items.Add(FieldByName('Name').AsString);
        Next;
      end;
    end;
    DBSelectBox.Initialise;

    { IntegratedDBSelectBox }
    IntegratedDBSelectBox.Initialise;
  finally
    Categories.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  IntegratedDBSelectBox.SelectElement(Edit1.Text);
end;

end.
