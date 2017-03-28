unit adsSelectionTreeViewComp;

{ © 2001-2010 Aston Design Studio }

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, SysUtils,
  adsSelectionCompTypes;

type
  TadsSelectionTreeView = class;

  {** Type for the OnSelectionChanged event. }
  TSelectionChangedEvent = procedure(Sender: TadsSelectionTreeView; Item: String)
    of object;

  {** This component combines a tree view and an edit box to give the user a
      flexible way of selecting an item, either by writing it into the edit box
      or selecting it from the tree view. }
  TadsSelectionTreeView = class(TPanel)
  private
    FEditMode: TEditMode;
    FOnSelectionChanged: TSelectionChangedEvent;
  protected
    Edit: TEdit;
    TreeView: TTreeView;
    LastSelection: String;
    function GetTreeViewItem: String;
    procedure SetTreeViewItem(Value: String);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure EditExit(Sender: TObject);
    procedure GenerateSelectionChanged;
    procedure CopySelectedItemToEdit;

    {** Return the tree node corresponding to the given string. If no such node
        exists, one is created (and returned) if the CanCreate parameter is True,
        otherwise (CanCreate=False, the default) nil is returned. }
    function StringToItem(Value: String; CanCreate: Boolean=False): TTreeNode;

    {** Return the string corresponding to the given tree node. }
    function ItemToString(Item: TTreeNode): String;
  public
    {** Set this property to one of the items in the list box to select that
        item. Read this property to access the selected item. }
    property Item: String read GetTreeViewItem write SetTreeViewItem;

    {** Clears the tree view. If the ClearEdit parameter is True (the default),
        the edit box is also cleared. }
    procedure ClearItems(ClearEdit: Boolean=True);

    {** Adds an item to the tree view. }
    procedure AddItem(Item: String);

    {** Adds the items given by Items to the list box. }
    procedure AddItems(Items: TStrings);

    {** Fill a TStrings object with all the strings in the tree view. }
    procedure GetItemsAsStrings(Strings: TStrings);

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published

    {** The component can be used in three different edit modes. If this property
        equals emStatic (default), the user is not allowed to select anything
        else than one of the items in the tree view. If the user leaves the edit
        box and its text does not correspond to one of the items in the tree view,
        its text is cleared and replaced with the latest selected item. If the
        property is emAllowFreeText, the user can enter anything in the edit box.
        When the user leaves the edit box, an event (OnSelectionChanged) is
        generated if the edit box contents have changed. If the property is
        emAllowFreeTextUpdating, the user can do just like in the case of
        emAllowFreeText, but the new entry is automatically added to the tree
        view for future use. }
    property EditMode: TEditMode read FEditMode write FEditMode;

    {** This event is generated when the selection has changed, i.e. when the
        user selects a new record in the tree view or leaves the edit component
        after changing its value. }
    property OnSelectionChanged: TSelectionChangedEvent
      read FOnSelectionChanged write FOnSelectionChanged;
  end{class};

implementation

{ Instead of including the StringMethods unit which is included in another
  package. }
procedure DelimitedStrToStrings(const S: string; Strings: TStrings; Delimiter:
  char);
var
  SubStrFirstPos, DelimiterPos, SLength: Integer;

  procedure NextDelimiterPos;
  begin
    repeat Inc(DelimiterPos)
    until (DelimiterPos > SLength) or (S[DelimiterPos] = Delimiter);
  end {procedure};

begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    SubStrFirstPos := 1;
    DelimiterPos := 0;
    SLength := Length(S);
    if SLength > 0 then
      repeat
        NextDelimiterPos;
        Strings.Add(Copy(S, SubStrFirstPos, DelimiterPos - SubStrFirstPos));
        SubStrFirstPos := DelimiterPos + 1;
      until DelimiterPos > SLength;
  finally
    Strings.EndUpdate;
  end {finally};
end {procedure};

function TadsSelectionTreeView.StringToItem(Value: String; CanCreate: Boolean=False):
  TTreeNode;

  function GetRootNode(NodeName: String; CanCreate: Boolean): TTreeNode;
  var
    i: Integer;
  begin
    Result := nil;
    with TreeView do
      for i := 0 to Items.Count-1 do
        if (TreeView.Items[i].Parent=nil) and (TreeView.Items[i].Text=NodeName) then
        begin
          Result := Items[i];
          Break
        end{if};
    if (Result = nil) and CanCreate then
      Result := TreeView.Items.Add(nil, NodeName);
  end{function};

  function GetChildNode(Node: TTreeNode; ChildNodeName: String;
    CanCreate: Boolean): TTreeNode;
  begin
    Result := Node.getFirstChild;
    if Result <> nil then
      repeat
        if Result.Text = ChildNodeName then
          Break;
        Result := Node.GetNextChild(Result);
      until Result = nil;
    if (Result = nil) and CanCreate then
      Result := TreeView.Items.AddChild(Node, ChildNodeName);
  end{function};

var
  i: Integer;
  ItemParts: TStrings;
begin
  Result := nil;
  if Value <> '' then
  begin
    if Value[1] = '\' then
      Value := Copy(Value, 2, Length(Value));
    ItemParts := TStringList.Create;
    try
      DelimitedStrToStrings(Value, ItemParts, '\');
      Result := GetRootNode(ItemParts[0], CanCreate);
      if Result <> nil then
        for i := 1 to ItemParts.Count-1 do
        begin
          Result := GetChildNode(Result, ItemParts[i], CanCreate);
          if Result = nil then
            Break;
        end{for};
    finally
      ItemParts.Free;
    end{try/finally};
  end{if};
end{function};

function TadsSelectionTreeView.ItemToString(Item: TTreeNode): String;
begin
  Result := Item.Text;
  while Item.Parent <> nil do
  begin
    Item := Item.Parent;
    Result := Item.Text + '\' + Result;
  end{while};
end{function};

function TadsSelectionTreeView.GetTreeViewItem: String;
begin
  Result := Edit.Text;
end{function};

procedure TadsSelectionTreeView.SetTreeViewItem(Value: String);
var
  Node: TTreeNode;
begin
  if Value <> '' then
  begin
    Node := StringToItem(Value);
    if Node <> nil then
    begin
      Edit.Text := Value;
      TreeView.Selected := Node;
    end{if};
  end{if};
end{procedure};

procedure TadsSelectionTreeView.CopySelectedItemToEdit;
begin
  Edit.Text := ItemToString(TreeView.Selected);
end{procedure};

procedure TadsSelectionTreeView.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  CopySelectedItemToEdit;
  GenerateSelectionChanged;
end{procedure};

procedure TadsSelectionTreeView.EditExit(Sender: TObject);

  function NodeExists(Name: String): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    with TreeView do
      for i := 0 to Items.Count-1 do
        if Items[i].Text=Name then
        begin
          Result := True;
          Break;
        end{if};
  end{function};

begin
  if not NodeExists(Edit.Text) then
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
          AddItem(Edit.Text); {Add to the tree view}
          Item := Edit.Text; {Select the new item}
        end{else};
      end{emAllowFreeTextUpdating};
    end{case};
  GenerateSelectionChanged;
end{procedure};

procedure TadsSelectionTreeView.ClearItems(ClearEdit: Boolean=True);
begin
  TreeView.Items.Clear;
  if ClearEdit then
    Edit.Text := '';
end{procedure};

procedure TadsSelectionTreeView.AddItem(Item: String);
begin
  StringToItem(Item, True);
end{procedure};

procedure TadsSelectionTreeView.AddItems(Items: TStrings);
var
  i: Integer;
begin
  for i := 0 to Items.Count-1 do
    StringToItem(Items[i], True);
end{procedure};

procedure TadsSelectionTreeView.GetItemsAsStrings(Strings: TStrings);
var
  i: Integer;
begin
  Strings.Clear;
  for i := 0 to TreeView.Items.Count-1 do
    Strings.Add(ItemToString(TreeView.Items[i]));
end{procedure};

procedure TadsSelectionTreeView.GenerateSelectionChanged;
begin
  if Edit.Text <> LastSelection then
  begin
    LastSelection := Edit.Text;
    if Assigned(OnSelectionChanged) then
      OnSelectionChanged(Self, Edit.Text);
  end{if};
end{procedure};

constructor TadsSelectionTreeView.Create(Owner: TComponent);
begin
  inherited;
  Edit := TEdit.Create(Self);
  Edit.Parent := Self;
  Edit.Top := 0;
  Edit.Align := alTop;
  Edit.OnExit := EditExit;
  TreeView := TTreeView.Create(Self);
  TreeView.Parent := Self;
  TreeView.Top := Edit.Height;
  TreeView.Align := alClient;
  TreeView.OnChange := TreeViewChange;
  TreeView.HideSelection := False;
  FEditMode := emStatic;
end{constructor};

destructor TadsSelectionTreeView.Destroy;
begin
  Edit.Free;
  TreeView.Free;
  inherited;
end{destructor};

end.