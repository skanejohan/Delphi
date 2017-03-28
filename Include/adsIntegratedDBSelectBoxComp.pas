unit adsIntegratedDBSelectBoxComp;

{ © 2001-2010 Aston Design Studio }

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, SysUtils, Db, adsDBSelectBoxComp;

type
  TElementSelectedEvent = procedure(Sender: TObject; Element: String) of object;

  {** The class for a select box with database support and a visual interface. }
  TadsIntegratedDBSelectBox = class(TPanel)
    ListBox: TListBox;
    Splitter: TSplitter;
    DBSelectBox: TadsDBSelectBox;
  protected
    FOnElementSelected: TElementSelectedEvent;
    function GetCategoryDataSource: TDataSource;
    procedure SetCategoryDataSource(Value: TDataSource);
    function GetCategoryDataSourceIDField: string;
    procedure SetCategoryDataSourceIDField(const Value: string);
    function GetCategoryDataSourceNameField: string;
    procedure SetCategoryDataSourceNameField(const Value: string);

    function GetCategoryElementDataSource: TDataSource;
    procedure SetCategoryElementDataSource(Value: TDataSource);
    function GetCategoryElementDataSourceCategoryIDField: string;
    procedure SetCategoryElementDataSourceCategoryIDField(const Value: string);
    function GetCategoryElementDataSourceElementIDField: string;
    procedure SetCategoryElementDataSourceElementIDField(const Value: string);

    function GetElementDataSource: TDataSource;
    procedure SetElementDataSource(Value: TDataSource);
    function GetElementDataSourceIDField: string;
    procedure SetElementDataSourceIDField(const Value: string);
    function GetElementDataSourceNameField: string;
    procedure SetElementDataSourceNameField(const Value: string);

    procedure SetListBoxHeight(Value: Integer);
    function GetListBoxHeight: Integer;

    procedure ListBoxClick(Sender: TObject);
    procedure GenerateClick(Sender: TObject);
    procedure GenerateKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GenerateDoubleClick(Sender: TObject);
  public

    {** Call Initialise to initalise the component to use the categories in
        the data set given by the CategoryDataSet and fields properties and the
        elements in the ElementDataSet and fields properties. Initialise can be
        called several times; each time the current selection is cleared and the
        categories and elements are changed. }
    procedure Initialise;

    {** Call this method to receive the categories for the given Element. If the
        element does not exist, an empty list is returned. }
    procedure GetCategories(Element: String; Categories: TStrings);

    {** Call this method to select the given element. This will give the same
        result as if the element was selected by the user. }
    procedure SelectElement(Element: String);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    {** The data source connected to the data set where the categories are
        stored. }
    property CategoryDataSource: TDataSource read GetCategoryDataSource
      write SetCategoryDataSource;

    {** The field in the category data set where the ID of the category is
        stored.
        @bug Currently one must manually enter this.}
    property CategoryDataSourceIDField: String
      read GetCategoryDataSourceIDField
      write SetCategoryDataSourceIDField;

    {** The field in the category data set where the name of the category is
        stored.
        @bug Currently one must manually enter this.}
    property CategoryDataSourceNameField: String
      read GetCategoryDataSourceNameField
      write SetCategoryDataSourceNameField;

    {** The data source connected to the data set where the elements are
        stored. }
    property ElementDataSource: TDataSource read GetElementDataSource
      write SetElementDataSource;

    {** The field in the element data set where the ID of the element is
        stored.
        @bug Currently one must manually enter this.}
    property ElementDataSourceIDField: String
      read GetElementDataSourceIDField
      write SetElementDataSourceIDField;

    {** The field in the element data set where the name of the element is
        stored.
        @bug Currently one must manually enter this.}
    property ElementDataSourceNameField: String
      read GetElementDataSourceNameField
      write SetElementDataSourceNameField;

    {** The data source connected to the data set where the element/category
        connections are stored. }
    property CategoryElementDataSource: TDataSource
      read GetCategoryElementDataSource
      write SetCategoryElementDataSource;

    {** The field in the category element data set where the ID of the category
        is stored.
        @bug Currently one must manually enter this.}
    property CategoryElementDataSourceCategoryIDField: String
      read GetCategoryElementDataSourceCategoryIDField
      write SetCategoryElementDataSourceCategoryIDField;

    {** The field in the category element data set where the ID of the element
        is stored.
        @bug Currently one must manually enter this.}
    property CategoryElementDataSourceElementIDField: String
      read GetCategoryElementDataSourceElementIDField
      write SetCategoryElementDataSourceElementIDField;

    property ListBoxHeight: Integer read GetListBoxHeight write SetListBoxHeight;

    {** This event is generated when a new element has been selected. }
    property OnElementSelected: TElementSelectedEvent
      read FOnElementSelected
      write FOnElementSelected;

    property OnKeyDown;

  end{class};

implementation

function TadsIntegratedDBSelectBox.GetCategoryDataSource: TDataSource;
begin
  Result := DBSelectBox.CategoryDataSource;
end{function};

procedure TadsIntegratedDBSelectBox.SetCategoryDataSource(Value: TDataSource);
begin
  DBSelectBox.CategoryDataSource := Value;
end{procedure};

function TadsIntegratedDBSelectBox.GetCategoryDataSourceIDField: string;
begin
  Result := DBSelectBox.CategoryDataSourceIDField;
end{function};

procedure TadsIntegratedDBSelectBox.SetCategoryDataSourceIDField(const Value: string);
begin
  DBSelectBox.CategoryDataSourceIDField := Value;
end{function};

function TadsIntegratedDBSelectBox.GetCategoryDataSourceNameField: string;
begin
  Result := DBSelectBox.CategoryDataSourceNameField;
end{function};

procedure TadsIntegratedDBSelectBox.SetCategoryDataSourceNameField(const Value: string);
begin
  DBSelectBox.CategoryDataSourceNameField := Value;
end{function};

function TadsIntegratedDBSelectBox.GetCategoryElementDataSource: TDataSource;
begin
  Result := DBSelectBox.CategoryElementDataSource;
end{function};

procedure TadsIntegratedDBSelectBox.SetCategoryElementDataSource(Value: TDataSource);
begin
  DBSelectBox.CategoryElementDataSource := Value;
end{procedure};

function TadsIntegratedDBSelectBox.GetCategoryElementDataSourceCategoryIDField: string;
begin
  Result := DBSelectBox.CategoryElementDataSourceCategoryIDField;
end{function};

procedure TadsIntegratedDBSelectBox.SetCategoryElementDataSourceCategoryIDField(const Value: string);
begin
  DBSelectBox.CategoryElementDataSourceCategoryIDField := Value;
end{function};

function TadsIntegratedDBSelectBox.GetCategoryElementDataSourceElementIDField: string;
begin
  Result := DBSelectBox.CategoryElementDataSourceElementIDField;
end{function};

procedure TadsIntegratedDBSelectBox.SetCategoryElementDataSourceElementIDField(const Value: string);
begin
  DBSelectBox.CategoryElementDataSourceElementIDField := Value;
end{function};

function TadsIntegratedDBSelectBox.GetElementDataSource: TDataSource;
begin
  Result := DBSelectBox.ElementDataSource;
end{function};

procedure TadsIntegratedDBSelectBox.SetElementDataSource(Value: TDataSource);
begin
  DBSelectBox.ElementDataSource := Value;
end{procedure};

function TadsIntegratedDBSelectBox.GetElementDataSourceIDField: string;
begin
  Result := DBSelectBox.ElementDataSourceIDField;
end{function};

procedure TadsIntegratedDBSelectBox.SetElementDataSourceIDField(const Value: string);
begin
  DBSelectBox.ElementDataSourceIDField := Value;
end{function};

function TadsIntegratedDBSelectBox.GetElementDataSourceNameField: string;
begin
  Result := DBSelectBox.ElementDataSourceNameField;
end{function};

procedure TadsIntegratedDBSelectBox.SetElementDataSourceNameField(const Value: string);
begin
  DBSelectBox.ElementDataSourceNameField := Value;
end{function};

procedure TadsIntegratedDBSelectBox.SetListBoxHeight(Value: Integer);
begin
  ListBox.Height := Value;
end{procedure};

function TadsIntegratedDBSelectBox.GetListBoxHeight: Integer;
begin
  Result := ListBox.Height;
end{function};

procedure TadsIntegratedDBSelectBox.Initialise;
var
  ABookmark: TBookmark;
begin
  DBSelectBox.Initialise;
  with DBSelectBox.ElementDataSource.DataSet do
  begin
    DisableControls;
    ABookmark := GetBookmark;
    ListBox.Items.Clear;
    First;
    while not Eof do
    begin
      ListBox.Items.Add(FieldByName(DBSelectBox.ElementDataSourceNameField).AsString);
      Next;
    end{while};
    GotoBookmark(ABookmark);
    FreeBookmark(ABookmark);
    EnableControls;
  end{with};
  if ListBox.Items.Count > 0 then
  begin
    ListBox.Selected[0] := True;
    ListBoxClick(Self);
  end{if};
end{procedure};

procedure TadsIntegratedDBSelectBox.GetCategories(Element: String;
  Categories: TStrings);
begin
  DBSelectBox.GetCategories(Element, Categories);
end{procedure};

procedure TadsIntegratedDBSelectBox.ListBoxClick(Sender: TObject);
var
  i: Integer;
  LastSelected: Integer;
begin
  LastSelected := -1;
  DBSelectBox.ClearSelection;
  for i := 0 to ListBox.Items.Count-1 do
    if ListBox.Selected[i] then
    begin
      DBSelectBox.SelectFor(ListBox.Items[i]);
      LastSelected := i;
    end{if};
  if (LastSelected <> -1) and Assigned(OnElementSelected) then
    OnElementSelected(Self, ListBox.Items[LastSelected]);
end{procedure};

procedure TadsIntegratedDBSelectBox.GenerateDoubleClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end{procedure};

procedure TadsIntegratedDBSelectBox.GenerateClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end{procedure};

procedure TadsIntegratedDBSelectBox.GenerateKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
end{procedure};

procedure TadsIntegratedDBSelectBox.SelectElement(Element: String);
var
  i: Integer;
begin
  with ListBox do
    for i := 0 to Items.Count-1 do
      Selected[i] := AnsiCompareText(Items[i], Element) = 0;

  DBSelectBox.ClearSelection;
  DBSelectBox.SelectFor(Element);
  ListBoxClick(Self);
end{procedure};

constructor TadsIntegratedDBSelectBox.Create(AOwner: TComponent);
begin
  inherited;
  ListBox := TListBox.Create(Self);
  ListBox.Parent := Self;
  ListBox.Align := alTop;
  ListBox.Height := Self.Height div 2;
  ListBox.MultiSelect := True;
  ListBox.OnClick := ListBoxClick;
  ListBox.OnKeyDown := GenerateKeyDown;
  ListBox.OnDblClick := GenerateDoubleClick;
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Align := alTop;
  Splitter.Top := ListBox.Height;
  DBSelectBox := TadsDBSelectBox.Create(Self);
  DBSelectBox.Parent := Self;
  DBSelectBox.Align := alClient;
  DBSelectBox.OnDblClick := GenerateDoubleClick;
  DBSelectBox.OnKeyDown := GenerateKeyDown;
end{constructor};

destructor TadsIntegratedDBSelectBox.Destroy;
begin
  DBSelectBox.Free;
  ListBox.Free;
  inherited;
end{destructor};

end.
