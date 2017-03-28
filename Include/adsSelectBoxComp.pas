unit adsSelectBoxComp;

{ © 2001-2010 Aston Design Studio }

interface

uses
  Classes, StdCtrls, Checklst;

type
  {** Type for the OnSelectionChanged event of TadsSelectBox. }
  TSelectionChangedEvent = procedure(Sender: TObject; Element, Category: String;
    Selected: Boolean) of object;

  TadsSelectBox = class;

  {** Class for an element selection, ie an element and the categories to which
      it belongs. }
  TElementSelection = class
  private
    FElement: String;
    FCategories: TStringList;
  public
    property Element: String read FElement;
    property Categories: TStringList read FCategories;
    constructor Create(Element: String; Categories: TStrings);
    destructor Destroy; override;
  end{class};

  {** Given a number of categories, this component can be used to determine
      which of those that apply to a certain element. }
  TadsSelectBox = class(TCheckListBox)
  private
    FOnSelectionChanged: TSelectionChangedEvent;
  protected
    Selected: Array of TCheckBoxState;
    ElementSelections: TList; { Of TElementSelection }
    procedure ElementCategoryChanged(ElementSelection: TElementSelection;
      Category: String; Selected: Boolean); virtual;
    procedure ClickCheck; override;
  public

    {** Call Initialise to initalise the component to use the categories given
        by Categories. Initialise can be called several times; each time the
        current selection is cleared and the categories are changed according to
        the value of the Categories parameter. }
    procedure Initialise(Categories: TStrings);

    {** Clears the current selection, ie returns the component to the state in
        which it was when the Initialise method had been called. }
    procedure ClearSelection; override;

    {** Tell the component to select categories for the given element. It does
        this by checking the checkboxes for the categories given by categories
        (which should be a subset of all available categories as determined when
        calling the Initialise method) and de-checking all others. If there are
        already elements for which the select box has a current selection (i.e.
        the latest method called was not Initialise or ClearSelection), this
        element is added to the select box's list of elements. In this case
        the checkboxes for categories used by all the elements are checked, the
        checkboxes for categories not used by any of the elements are not
        checked and checkboxes for categories used by some but not all of the
        elements are checked and grayed. }
    procedure SelectFor(Element: String; Categories: TStrings); overload;
    {** In this version, the Categories parameter represents all categories that
     should be selected, separated by ';'. }
    procedure SelectFor(Element: String; Categories: String); overload;

    {** Call this method to receive the categories for the given Element. If the
        element does not exist, an empty list is returned. }
    procedure GetCategories(TheElement: String; TheCategories: TStrings); virtual;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    {** This event is generated when the category selection for an element is
        changed. If several elements are selected and the selection changes for
        more than one, one event is generated for each element. }
    property OnSelectionChanged: TSelectionChangedEvent read FOnSelectionChanged
      write FOnSelectionChanged;
  end{class};

implementation

uses
  adsStringMethods;

{---------- Implementation of TElementSelection ----------}

constructor TElementSelection.Create(Element: String; Categories: TStrings);
begin
  inherited Create;
  FCategories := TStringList.Create;
  FElement := Element;
  FCategories.Assign(Categories);
end{constructor};

destructor TElementSelection.Destroy;
begin
  FCategories.Free;
  inherited;
end{destructor};

{---------- Implementation of TadsSelectBox ----------}

procedure TadsSelectBox.Initialise(Categories: TStrings);
var
  i: Integer;
begin
  Items.Clear;
  SetLength(Selected, Categories.Count);
  for i := 0 to Categories.Count-1 do
  begin
    Items.Add(Categories[i]);
    State[i] := cbUnchecked;
    Selected[i] := cbUnchecked;
  end{for};
end{procedure};

procedure TadsSelectBox.ClearSelection;
var
  i: Integer;
begin
  for i := 0 to ElementSelections.Count-1 do
    TElementSelection(ElementSelections[i]).Free;
  ElementSelections.Clear;
  for i := 0 to Items.Count-1 do
  begin
    Selected[i] := cbUnchecked;
    State[i] := cbUnchecked;
  end{for};
end{procedure};

procedure TadsSelectBox.SelectFor(Element: String; Categories: TStrings);
var
  i, C: Integer;

  procedure SelectForIndex(Index: Integer; Used: Boolean);
  begin
    if Used then
    begin
      if ElementSelections.Count=1 then {This is the only file}
      begin
        State[Index] := cbChecked;
        Selected[Index] := cbChecked;
      end {if}
      else
        if State[Index] = cbUnchecked then
        begin
          State[Index] := cbGrayed;
          Selected[Index] := cbGrayed;
        end{if};
    end {if}
    else {not used}
      if State[Index] = cbChecked then
      begin
        State[Index] := cbGrayed;
        Selected[Index] := cbGrayed;
      end{if};
  end{procedure};

begin
  C := ElementSelections.Count;
  ElementSelections.Add(TElementSelection.Create(Element, Categories));
  if C <> ElementSelections.Count then {Otherwise Element was already in Elements}
    for i := 0 to Items.Count-1 do
      SelectForIndex(i, Categories.IndexOf(Items[i]) <> -1);
end{procedure};

procedure TadsSelectBox.SelectFor(Element: String; Categories: String);
var
  CategoryList: TStringList;
begin
  CategoryList := TStringList.Create;
  try
    DelimitedTextToStrings(Categories, CategoryList, ';');
    SelectFor(Element, CategoryList);
  finally
    CategoryList.Free;
  end{try/finally};
end{procedure};

procedure TadsSelectBox.GetCategories(TheElement: String; TheCategories: TStrings);
var
  i: Integer;
begin
  TheCategories.Clear;
  for i := 0 to ElementSelections.Count-1 do
    with TElementSelection(ElementSelections[i]) do
      if Element = TheElement then
      begin
        TheCategories.Assign(Categories);
        Break;
      end{if};
end{procedure};

procedure TadsSelectBox.ElementCategoryChanged(ElementSelection: TElementSelection;
  Category: String; Selected: Boolean);
begin
  with ElementSelection do
  begin
    if Selected then
      Categories.Add(Category)
    else
      Categories.Delete(Categories.IndexOf(Category));
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self, Element, Category, Selected);
  end{with};
end{procedure};

procedure TadsSelectBox.ClickCheck;
var
  i, j: Integer;
begin
  for i := 0 to Items.Count-1 do
    if State[i] <> Selected[i] then
    begin
      Selected[i] := State[i];
      for j := 0 to ElementSelections.Count-1 do
        with TElementSelection(ElementSelections[j]) do
          if (State[i] = cbChecked) and (Categories.IndexOf(Items[i]) = -1) then
            ElementCategoryChanged(ElementSelections[j], Items[i], True)
          else if (State[i] = cbUnchecked) and (Categories.IndexOf(Items[i]) <> -1) then
            ElementCategoryChanged(ElementSelections[j], Items[i], False);
    end{if};
  inherited;
end{procedure};

constructor TadsSelectBox.Create(AOwner: TComponent);
begin
  inherited;
  ElementSelections := TList.Create;
end{constructor};

destructor TadsSelectBox.Destroy;
var
  i: Integer;
begin
  for i := 0 to ElementSelections.Count-1 do
    TElementSelection(ElementSelections[i]).Free;
  ElementSelections.Free;
  inherited;
end{destructor};

end.
