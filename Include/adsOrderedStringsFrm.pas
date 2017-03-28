unit adsOrderedStringsFrm;

{ © 2001-2010 Aston Design Studio }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, StdCtrls, Menus;

type
  TItemSelectedEvent = procedure(Sender: TObject; Item: String) of object;

  {** This dialog allows the user to maintain on ordered set of strings. It may
      be used to add strings, move them up/down and save/load to/from a file. }
  TadsOrderedStringsForm = class(TForm)
    StringsListBox: TListBox;
    PopupMenu1: TPopupMenu;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    N1: TMenuItem;
    Deleteselecteditems1: TMenuItem;
    Clearall1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    N2: TMenuItem;
    Save1: TMenuItem;
    Load1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Moveup1Click(Sender: TObject);
    procedure Movedown1Click(Sender: TObject);
    procedure Deleteselecteditems1Click(Sender: TObject);
    procedure Clearall1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure StringsListBoxClick(Sender: TObject);
  private
    FAllowDuplicates: Boolean;
    FEnableSaveAndLoadItems: Boolean;
    FOnItemSelected: TItemSelectedEvent;
    procedure SetEnableSaveAndLoadItems(Value: Boolean);
  public

    {** Set AllowDuplicates to False if you want the form to reject all attempts
        to add a string that already exists. If AllowDuplicates is True, several
        instances of the same string may be inserted. Note that setting
        AllowDuplicates to False does not perform any control of the data
        already present in the form, only data that is added. Default is False. }
    property AllowDuplicates: Boolean read FAllowDuplicates
      write FAllowDuplicates;

    {** Determine whether the "Load" and "Save" items should be available in the
        popup menu. Default is true. }
    property EnableSaveAndLoadItems: Boolean read FEnableSaveAndLoadItems
      write SetEnableSaveAndLoadItems;

    {** Generated when the user clicks on an item in the form. }
    property OnItemSelected: TItemSelectedEvent read FOnItemSelected
      write FOnItemSelected;

    {** Adds the given string to the end of the grid. }
    procedure AddString(const AString: String);

    {** Inserts the given string at the given position in the grid. If Index is
        higher than the number of elements in the grid, the string is added to
        the end of the string. }
    procedure InsertString(const AString: String; Index: Integer);

    {** Load the contents of the grid from the file with the given name. }
    procedure Load(FileName: String);

    {** Save the contents of the grid to the file with the given name. }
    procedure Save(FileName: String);
  end{class};

var
  adsOrderedStringsForm: TadsOrderedStringsForm;

implementation

uses
  adsDialogMethods;
  
{$R *.DFM}

procedure TadsOrderedStringsForm.FormCreate(Sender: TObject);
begin
  FAllowDuplicates := False;
  FEnableSaveAndLoadItems := True;
end{procedure};

procedure TadsOrderedStringsForm.SetEnableSaveAndLoadItems(Value: Boolean);
begin
  FEnableSaveAndLoadItems := Value;
  N2.Visible := FEnableSaveAndLoadItems;
  Save1.Visible := FEnableSaveAndLoadItems;
  Load1.Visible := FEnableSaveAndLoadItems;
end{procedure};

procedure TadsOrderedStringsForm.AddString(const AString: String);
begin
  if AllowDuplicates or (StringsListBox.Items.IndexOf(AString) = -1) then
    StringsListBox.Items.Add(AString);
end{procedure};

procedure TadsOrderedStringsForm.InsertString(const AString: String; Index: Integer);
begin
  if Index > StringsListBox.Items.Count then
    Index := StringsListBox.Items.Count;
  if AllowDuplicates or (StringsListBox.Items.IndexOf(AString) = -1) then
    StringsListBox.Items.Insert(Index, AString);
end{procedure};

procedure TadsOrderedStringsForm.Load(FileName: String);
begin
  StringsListBox.Items.LoadFromFile(FileName);
end{procedure};

procedure TadsOrderedStringsForm.Save(FileName: String);
begin
  StringsListBox.Items.SaveToFile(FileName);
end{procedure};

procedure TadsOrderedStringsForm.Moveup1Click(Sender: TObject);
var
  TempItem: String;
  Ctr: Integer;
begin
  with StringsListBox do
    if not Selected[0] then
    begin
      Ctr := 0;
      while Ctr < Items.Count do
      begin
        while (Ctr < Items.Count)  and (not Selected[Ctr]) do
          Inc(Ctr);
        TempItem := Items[Ctr-1];
        while (Ctr < Items.Count) and (Selected[Ctr]) do
        begin
          Items[Ctr-1] := Items[Ctr];
          Selected[Ctr-1] := True;
          Inc(Ctr);
        end{while};
        Items[Ctr-1] := TempItem;
      end{while};
    end{if};
end{procedure};

procedure TadsOrderedStringsForm.Movedown1Click(Sender: TObject);
var
  TempItem: String;
  Ctr: Integer;
begin
  with StringsListBox do
    if not Selected[Items.Count-1] then
    begin
      Ctr := Items.Count-1;
      while Ctr > -1 do
      begin
        while (Ctr > -1)  and (not Selected[Ctr]) do
          Dec(Ctr);
        TempItem := Items[Ctr+1];
        while (Ctr > -1) and (Selected[Ctr]) do
        begin
          Items[Ctr+1] := Items[Ctr];
          Selected[Ctr+1] := True;
          Dec(Ctr);
        end{while};
        Items[Ctr+1] := TempItem;
      end{while};
    end{if};
end{procedure};

procedure TadsOrderedStringsForm.Deleteselecteditems1Click(
  Sender: TObject);
var
  i: Integer;
begin
  if ConfirmationDialog(Self, 'Really delete all selected items?') = mrYes then
    with StringsListBox, StringsListBox.Items do
      for i := Count-1 downto 0 do
        if Selected[i] then
          Delete(i);
end{procedure};

procedure TadsOrderedStringsForm.Clearall1Click(Sender: TObject);
begin
  if ConfirmationDialog(Self, 'Really clear?') = mrYes then
    StringsListBox.Items.Clear;
end{procedure};

procedure TadsOrderedStringsForm.Save1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
    Save(SaveDialog.FileName);
end{procedure};

procedure TadsOrderedStringsForm.Load1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    Load(OpenDialog.FileName);
end{procedure};

procedure TadsOrderedStringsForm.StringsListBoxClick(Sender: TObject);
begin
  if Assigned(OnItemSelected) then
    OnItemSelected(Self, StringsListBox.Items[StringsListBox.ItemIndex]);
end{procedure};

end.
