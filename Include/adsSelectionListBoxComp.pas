unit adsSelectionListBoxComp;

{ © 2001-2010 Aston Design Studio }

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, SysUtils,
  adsSelectionCompTypes;

type
  TadsSelectionListBox = class;

  {** Type for the OnSelectionChanged event. }
  TSelectionChangedEvent = procedure(Sender: TadsSelectionListBox; Item: String)
    of object;

  {** This component combines a list box and an edit box to give the user a
      flexible way of selecting an item, either by writing it into the edit box
      or selecting it from the list box. }
  TadsSelectionListBox = class(TPanel)
  private
    FSorted: Boolean;
    FEditMode: TEditMode;
    FFirstItem: String;
    FOnSelectionChanged: TSelectionChangedEvent;
  protected
    Edit: TEdit;
    ListBox: TListBox;
    LastSelection: String;
    function GetListItem: String;
    procedure SetListItem(Value: String);
    procedure ListBoxClick(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure PossiblySortAndSetFirstItem;
    procedure GenerateSelectionChanged;
    procedure CopySelectedItemToEdit;
  public
    {** Set this property to one of the items in the list box to select that
        item. Read this property to access the selected item. }
    property Item: String read GetListItem write SetListItem;

    {** Clears all items in the list box. If the ClearFirst parameter is True
        (the default), FirstItem is also cleared. If the ClearEdit parameter is
        True (the default), the edit box is also cleared. }
    procedure ClearItems(ClearFirstItem: Boolean=True; ClearEdit: Boolean=True);

    {** Adds an item to the list box. If sorted is True, it is inserted in its
        alphabetically correct position in the list, otherwise it is inserted
        last. }
    procedure AddItem(Item: String);

    {** Adds the items given by Items to the list box. If sorted is True, each
        item is inserted in its alphabetically correct position in the list,
        otherwise the items are inserted last. }
    procedure AddItems(Items: TStrings);

    {** Fill a TStrings object with all the strings in the tree view. }
    procedure GetItemsAsStrings(Strings: TStrings);

    {** If you want one of the items in the list to appear first in the list
        even though it normally wouldn't, because the Sorted property is True,
        enter that item here. If this property is '' (the default), all items
        are treated the same way. Note that this property has no effect if the
        Sorted property is False. }
    property FirstItem: String read FFirstItem write FFirstItem;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published

    {** The component can be used in three different edit modes. If this property
        equals emStatic (default), the user is not allowed to select anything
        else than one of the items in the list box. If the user leaves the edit
        box and its text does not correspond to one of the items in the list box,
        its text is cleared and replaced with the latest selected item. If the
        property is emAllowFreeText, the user can enter anything in the edit box.
        When the user leaves the edit box, an event (OnSelectionChanged) is
        generated if the edit box contents have changed. If the property is
        emAllowFreeTextUpdating, the user can do just like in the case of
        emAllowFreeText, but the new entry is automatically added to the list
        box for future use. }
    property EditMode: TEditMode read FEditMode write FEditMode;

    {** If this property is True (the default), the list box items are
        automatically sorted alphabetically. See however FirstItem. }
    property Sorted: Boolean read FSorted write FSorted;

    {** This event is generated when the selection has changed, i.e. when the
        user selects a new record in the list box or leaves the edit component
        after changing its value. }
    property OnSelectionChanged: TSelectionChangedEvent
      read FOnSelectionChanged write FOnSelectionChanged;
  end{class};

implementation

function TadsSelectionListBox.GetListItem: String;
begin
  Result := Edit.Text;
end{function};

procedure TadsSelectionListBox.SetListItem(Value: String);
begin
  if Value <> '' then
  begin
    Edit.Text := Value;
    with ListBox do
      if Items.IndexOf(Item) <> -1 then
        ItemIndex := Items.IndexOf(Item);
  end{if};
end{procedure};

procedure TadsSelectionListBox.CopySelectedItemToEdit;
var
  i: Integer;
begin
  for i := 0 to ListBox.Items.Count-1 do
    if ListBox.Selected[i] then
    begin
      Edit.Text := ListBox.Items[i];
      Break;
    end{if};
end{procedure};

procedure TadsSelectionListBox.ListBoxClick(Sender: TObject);
begin
  CopySelectedItemToEdit;
  GenerateSelectionChanged;
end{procedure};

procedure TadsSelectionListBox.EditExit(Sender: TObject);
begin
  if ListBox.Items.IndexOf(Edit.Text) = -1 then
    case FEditMode of
      emStatic:
        CopySelectedItemToEdit; {Cancel the changes.}
      emAllowFreeText:
        if Edit.Text = '' then
          CopySelectedItemToEdit; {Cancel the changes if the edit is empty.}
      emAllowFreeTextUpdating:
      begin
        if Edit.Text = '' then
          CopySelectedItemToEdit {Cancel the changes if the edit is empty.}
        else {The change was OK.}
        begin
          AddItem(Edit.Text); {Add to the list box}
          Item := Edit.Text; {Select the new item}
        end{else};
      end{emAllowFreeTextUpdating};
    end{case};
  GenerateSelectionChanged;
end{procedure};

procedure TadsSelectionListBox.EditChange(Sender: TObject);
var
  i: Integer;
begin
  if (Edit.Text = '') and (ListBox.Items.Count > 0) then
    ListBox.ItemIndex := 0;
  for i := 0 to ListBox.Items.Count-1 do
    if Pos(AnsiUpperCase(Edit.Text), AnsiUpperCase(ListBox.Items[i])) = 1 then
    begin
      ListBox.ItemIndex := i;
      Break;
    end{if};
end{procedure};

procedure TadsSelectionListBox.PossiblySortAndSetFirstItem;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.AddStrings(ListBox.Items);
    if FSorted then
      SL.Sort;
    if SL.IndexOf(FFirstItem) <> -1 then
    begin
      SL.Delete(SL.IndexOf(FFirstItem));
      SL.Insert(0, FFirstItem);
    end{if};
    ListBox.Items.Clear;
    ListBox.Items.AddStrings(SL);
  finally
    SL.Free;
  end{try/finally};
end{procedure};

procedure TadsSelectionListBox.ClearItems(ClearFirstItem: Boolean=True;
  ClearEdit: Boolean=True);
begin
  ListBox.Items.Clear;
  if ClearFirstItem then
    FFirstItem := '';
  if ClearEdit then
    Edit.Text := '';
end{procedure};

procedure TadsSelectionListBox.AddItem(Item: String);
begin
  ListBox.Items.Add(Item);
  PossiblySortAndSetFirstItem;
end{procedure};

procedure TadsSelectionListBox.AddItems(Items: TStrings);
var
  i: Integer;
begin
  for i := 0 to Items.Count-1 do
    ListBox.Items.Add(Items[i]);
  PossiblySortAndSetFirstItem;
end{procedure};

procedure TadsSelectionListBox.GetItemsAsStrings(Strings: TStrings);
begin
  Strings.Clear;
  Strings.AddStrings(ListBox.Items);
end{procedure};

procedure TadsSelectionListBox.GenerateSelectionChanged;
begin
  if Edit.Text <> LastSelection then
  begin
    LastSelection := Edit.Text;
    if Assigned(OnSelectionChanged) then
      OnSelectionChanged(Self, Edit.Text);
  end{if};
end{procedure};

constructor TadsSelectionListBox.Create(Owner: TComponent);
begin
  inherited;
  Edit := TEdit.Create(Self);
  Edit.Parent := Self;
  Edit.Top := 0;
  Edit.Align := alTop;
  Edit.OnExit := EditExit;
  Edit.OnChange := EditChange;
  ListBox := TListBox.Create(Self);
  ListBox.Parent := Self;
  ListBox.Top := Edit.Height;
  ListBox.Align := alClient;
  ListBox.OnClick := ListBoxClick;
  FFirstItem := '';
  FSorted := True;
  FEditMode := emStatic;
end{constructor};

destructor TadsSelectionListBox.Destroy;
begin
  Edit.Free;
  ListBox.Free;
  inherited;
end{destructor};

end.
