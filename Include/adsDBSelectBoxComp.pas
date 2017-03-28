unit adsDBSelectBoxComp;

{ © 2001-2010 Aston Design Studio }

interface

uses
  Classes, Db, DbCtrls, SysUtils, adsSelectBoxComp, DbClient, Variants, Provider;

type

  {** Given a number of categories, this component can be used to determine
      which of those that apply to a certain element. This component works like
      the TSelectBox component but adds database support. }
  TadsDBSelectBox = class(TadsSelectBox)
  private
    FCategoryDataSourceIDLink: TFieldDataLink;
    FCategoryDataSourceNameLink: TFieldDataLink;
    FCategoryElementDataSourceCategoryIDLink: TFieldDataLink;
    FCategoryElementDataSourceElementIDLink: TFieldDataLink;
    FElementDataSourceIDLink: TFieldDataLink;
    FElementDataSourceNameLink: TFieldDataLink;
  protected
    UnfilteredElementDataSet: TClientDataSet;

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

    function ElementDataSet: TDataSet;
    function CategoryDataSet: TDataSet;
    function CategoryElementDataSet: TDataSet;

    procedure ElementCategoryChanged(ElementSelection: TElementSelection;
      Category: String; Selected: Boolean); override;
  public
    {** Call Initialise to initalise the component to use the categories in
        the data set given by the CategoryDataSet and CategoryField properties.
        Initialise can be called several times; each time the current selection
        is cleared and the categories are changed according to the value of the
        CategoryDataSet and CategoryField properties. }
    procedure Initialise;

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
    procedure SelectFor(Element: String);

    procedure GetCategories(TheElement: String; TheCategories: TStrings); override;

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
  end{class};

implementation

procedure TadsDBSelectBox.Initialise;
var
  Categories: TStrings;
  TempProvider: TDataSetProvider;
begin
  Categories := TStringList.Create;
  try
    with CategoryDataSet do
    begin
      First;
      while not Eof do
      begin
        Categories.Add(FCategoryDataSourceNameLink.Field.AsString);
        Next;
      end{while};
    end{with};
    with ElementDataSet do
    begin
      Filtered := False;

      { Now copy all data in ElementDataSet into UnfilteredElementDataSet. This is
        because we want to be able to ask for the categories also for elements that
        may currently be unavailable due to filtering of ElementDataSet. }
      TempProvider := TDataSetProvider.Create(Self);
      try
        TempProvider.DataSet := ElementDataSet;
        UnfilteredElementDataSet.Data := TempProvider.Data;
      finally
        TempProvider.Free;
      end{try/finally};

      Filtered := True;
    end{with};

    inherited Initialise(Categories);
  finally
    Categories.Free;
  end{try/finally};
end{procedure};

procedure TadsDBSelectBox.SelectFor(Element: String);
var
  ElementID: Integer;
  Categories: TStrings;
begin
  Categories := TStringList.Create;
  try
    if ElementDataSet.Locate(FElementDataSourceNameLink.FieldName, Element, []) then
    begin
      ElementID := FElementDataSourceIDLink.Field.AsInteger;
      with CategoryElementDataSet do
      begin
        Filtered := True;
        Filter := FCategoryElementDataSourceElementIDLink.FieldName + '=' +
          IntToStr(ElementID);
        First;
        while not Eof do
        begin
          CategoryDataSet.Locate(FCategoryDataSourceIDLink.FieldName,
            FCategoryElementDataSourceCategoryIDLink.Field.AsInteger, []);
          Categories.Add(FCategoryDataSourceNameLink.Field.AsString);
          Next;
        end{while};
      end{with};
      inherited SelectFor(Element, Categories);
    end{if};
  finally
    Categories.Free;
  end{try/finally};
end{procedure};

procedure TadsDBSelectBox.GetCategories(TheElement: String; TheCategories: TStrings);
var
  ElementID: Integer;
  IDFieldName: String;
begin
  TheCategories.Clear;
  if UnfilteredElementDataSet.Locate(FElementDataSourceNameLink.FieldName,
    TheElement, [loCaseInsensitive]) then
  begin
    IDFieldName := FElementDataSourceIDLink.FieldName;
    ElementID := UnfilteredElementDataSet.FieldByName(IDFieldName).AsInteger;
    with CategoryElementDataSet do
    begin
      Filtered := True;
      Filter := FCategoryElementDataSourceElementIDLink.FieldName + '=' +
        IntToStr(ElementID);
      First;
      while not Eof do
      begin
        CategoryDataSet.Locate(FCategoryDataSourceIDLink.FieldName,
          FCategoryElementDataSourceCategoryIDLink.Field.AsInteger, []);
        TheCategories.Add(FCategoryDataSourceNameLink.Field.AsString);
        Next;
      end{while};
    end{with};
  end{if};
end{procedure};

procedure TadsDBSelectBox.ElementCategoryChanged(ElementSelection: TElementSelection;
  Category: String; Selected: Boolean);
var
  ElementID, CategoryID: Integer;
begin
  if ElementDataSet.Locate(FElementDataSourceNameLink.FieldName, ElementSelection.Element, []) then
  begin
    ElementID := FElementDataSourceIDLink.Field.AsInteger;
    if CategoryDataSet.Locate(FCategoryDataSourceNameLink.Field.FieldName, Category, []) then
    begin
      CategoryID := FCategoryDataSourceIDLink.Field.AsInteger;
      if Selected then
      begin
        if not CategoryElementDataSet.Locate(
          FCategoryElementDataSourceElementIDLink.FieldName+';'+
          FCategoryElementDataSourceCategoryIDLink.FieldName,
          VarArrayOf([ElementID, CategoryID]), []) then
            CategoryElementDataSet.AppendRecord([ElementID, CategoryID]);
      end {if}
      else
      begin
        if CategoryElementDataSet.Locate(
          FCategoryElementDataSourceElementIDLink.FieldName+';'+
          FCategoryElementDataSourceCategoryIDLink.FieldName,
          VarArrayOf([ElementID, CategoryID]), []) then
            CategoryElementDataSet.Delete;
      end{else};
    end{if};
  end{if};
  inherited;
end{procedure};

function TadsDBSelectBox.GetCategoryDataSource: TDataSource;
begin
  Result := FCategoryDataSourceIDLink.DataSource;
end{function};

procedure TadsDBSelectBox.SetCategoryDataSource(Value: TDataSource);
begin
  FCategoryDataSourceIDLink.DataSource := Value;
  FCategoryDataSourceNameLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end{procedure};

function TadsDBSelectBox.GetCategoryDataSourceIDField: string;
begin
  Result := FCategoryDataSourceIDLink.FieldName;
end{function};

procedure TadsDBSelectBox.SetCategoryDataSourceIDField(const Value: string);
begin
  FCategoryDataSourceIDLink.FieldName := Value;
end{function};

function TadsDBSelectBox.GetCategoryDataSourceNameField: string;
begin
  Result := FCategoryDataSourceNameLink.FieldName;
end{function};

procedure TadsDBSelectBox.SetCategoryDataSourceNameField(const Value: string);
begin
  FCategoryDataSourceNameLink.FieldName := Value;
end{function};

function TadsDBSelectBox.GetCategoryElementDataSource: TDataSource;
begin
  Result := FCategoryElementDataSourceCategoryIDLink.DataSource;
end{function};

procedure TadsDBSelectBox.SetCategoryElementDataSource(Value: TDataSource);
begin
  FCategoryElementDataSourceCategoryIDLink.DataSource := Value;
  FCategoryElementDataSourceElementIDLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end{procedure};

function TadsDBSelectBox.GetCategoryElementDataSourceCategoryIDField: string;
begin
  Result := FCategoryElementDataSourceCategoryIDLink.FieldName;
end{function};

procedure TadsDBSelectBox.SetCategoryElementDataSourceCategoryIDField(const Value: string);
begin
  FCategoryElementDataSourceCategoryIDLink.FieldName := Value;
end{function};

function TadsDBSelectBox.GetCategoryElementDataSourceElementIDField: string;
begin
  Result := FCategoryElementDataSourceElementIDLink.FieldName;
end{function};

procedure TadsDBSelectBox.SetCategoryElementDataSourceElementIDField(const Value: string);
begin
  FCategoryElementDataSourceElementIDLink.FieldName := Value;
end{function};

function TadsDBSelectBox.GetElementDataSource: TDataSource;
begin
  Result := FElementDataSourceIDLink.DataSource;
end{function};

procedure TadsDBSelectBox.SetElementDataSource(Value: TDataSource);
begin
  FElementDataSourceIDLink.DataSource := Value;
  FElementDataSourceNameLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end{procedure};

function TadsDBSelectBox.GetElementDataSourceIDField: string;
begin
  Result := FElementDataSourceIDLink.FieldName;
end{function};

procedure TadsDBSelectBox.SetElementDataSourceIDField(const Value: string);
begin
  FElementDataSourceIDLink.FieldName := Value;
end{function};

function TadsDBSelectBox.GetElementDataSourceNameField: string;
begin
  Result := FElementDataSourceNameLink.FieldName;
end{function};

procedure TadsDBSelectBox.SetElementDataSourceNameField(const Value: string);
begin
  FElementDataSourceNameLink.FieldName := Value;
end{function};

function TadsDBSelectBox.ElementDataSet: TDataSet;
begin
  Result := FElementDataSourceIDLink.DataSet;
end{function};

function TadsDBSelectBox.CategoryDataSet: TDataSet;
begin
  Result := FCategoryDataSourceIDLink.DataSet;
end{function};

function TadsDBSelectBox.CategoryElementDataSet: TDataSet;
begin
  Result := FCategoryElementDataSourceCategoryIDLink.DataSet;
end{function};

constructor TadsDBSelectBox.Create(AOwner: TComponent);
begin
  inherited;
  FCategoryDataSourceIDLink := TFieldDataLink.Create;
  FCategoryDataSourceNameLink := TFieldDataLink.Create;
  FCategoryElementDataSourceCategoryIDLink := TFieldDataLink.Create;
  FCategoryElementDataSourceElementIDLink := TFieldDataLink.Create;
  FElementDataSourceIDLink := TFieldDataLink.Create;
  FElementDataSourceNameLink := TFieldDataLink.Create;
  UnfilteredElementDataSet := TClientDataSet.Create(Self);
end{constructor};

destructor TadsDBSelectBox.Destroy;
begin
  UnfilteredElementDataSet.Free;
  FCategoryDataSourceIDLink.Free;
  FCategoryDataSourceNameLink.Free;
  FCategoryElementDataSourceCategoryIDLink.Free;
  FCategoryElementDataSourceElementIDLink.Free;
  FElementDataSourceIDLink.Free;
  FElementDataSourceNameLink.Free;
  inherited;
end{destructor};

end.
