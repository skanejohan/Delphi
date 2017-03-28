unit adsContainers;

{
  © 2007 Aston Design Studio

  TadsSortedList is a replacement of the standard TList class that has a number
   of methods for handling sorting and searching in an effective way.

  TadsIntegerList is a special case of TadsSortedList, suitable for handling
  integers.
}

interface

uses
  Classes;

type
  TadsSortedList = class(TList)
  public

    {** Adds the specified Item in the list in sorted order. This method takes
     time proportional to Count in the worst case, so if you are sequentially
     adding a large number of items to the list, it is faster to use the normal
     Add function instead and then call Sort when you have added all items.
     Returns the index the item was inserted on.
     @param Item The item that should be inserted.
     @param Compare is a function that compares the data that two pointers Item1
      and Item 2 references and returns < 0 if Item1's data is less than Item2's,
      0 if they are equal and > 0 if Item1's data is greater than Item2's.}
    function AddSorted(Item: Pointer; Compare: TListSortCompare): Integer;

    {** Returns the index of an item in the list that matches the data content
     of a specified item, assuming the list is sorted in ascending order, or -1
     if no such item is in the list. Note that this can be equivalent to finding
     the exact same item (i.e. same pointer value) as in the normal IndexOf
     function, but it does not have to be. Using this function is quicker (takes
     time proportional to log(Count)) than IndexOf, but it can only be used if
     the items in the list are sorted.
     @param Item An item that has the data content you are searching for.
     @param Compare is a function that compares the data that two pointers Item1
      and Item 2 references and returns < 0 if Item1's data is less than Item2's,
      0 if they are equal and > 0 if Item1's data is greater than Item2's.}
    function IndexOfSorted(Item: Pointer; Compare: TListSortCompare): Integer;

    {** Sorts the items in the list in ascending order using a QuickSort
     algorithm. This operation takes time proportional to Count * log(Count) on
     average. Does nothing if the list is empty.
     @param Compare is a function that compares the data that two pointers Item1
      and Item 2 references and returns < 0 if Item1's data is less than Item2's,
      0 if they are equal and > 0 if Item1's data is greater than Item2's.}
    procedure Sort(Compare: TListSortCompare);

    {** Works like Delete for TList, but if the optional DeleteObject parameter
     is True, it also frees the memory for the deleted object. Note: If the data
     at the given index is not of type TObject or a derived class, do not set
     DeleteObject to True! }
    procedure Delete(Index: Integer; DeleteObject: Boolean = False);

    {** Works like Clear for TList, but also frees the memory for all deleted
     objects. Note: If the data in the list is not of type TObject or a derived
     class, use Clear instead! }
    procedure ClearObjects;

  end; {public}

  TadsIntegerList = class(TadsSortedList)
  private
    function GetItems(Index: Integer): Integer;
    procedure SetItems(Index: Integer; const Value: Integer);
  public

    {** Works like TList.Add, but for integers. }
    function Add(Item: Integer): Integer;

    {** Adds the specified Item in the list in sorted order. This method takes
     time proportional to Count in the worst case, so if you are sequentially
     adding a large number of items to the list, it is faster to use the normal
     Add function instead and then call Sort when you have added all items.
     Returns the index the item was inserted on.}
    function AddSorted(Item: Integer): Integer;

    {** Works like TList.IndexOf, but for integers. }
    function IndexOf(Item: Integer): Integer;

    {** Returns the index of the specified item in the list, assuming the list
     is sorted in ascending order, or -1 if it is not in the list. Using this
     function is quicker (takes time proportional to log(Count)) than IndexOf,
     but it can only be used if the items in the list are sorted.}
    function IndexOfSorted(Item: Integer): Integer;

    {** Works like TList.Remove, but for integers. }
    function Remove(Item: Integer): Integer;

    {** Works like TList.Insert, but for integers. }
    procedure Insert(Index: Integer; Item: Integer);

    {** Works like TList.Delete. }
    procedure Delete(Index: Integer);

    {** Sorts the items in the list in ascending order using a QuickSort
     algorithm. This operation takes time proportional to Count * log(Count) on
     average. Does nothing if the list is empty.}
    procedure Sort;

    {** Works like TList.Items, but for integers. }
    property Items[Index: Integer]: Integer read GetItems write SetItems; default;

  end; {class}

implementation

{---------- Search and sort methods -------------------------------------------}

function BinarySearch(Item: Pointer; List: TList; const Count: Integer;
  var Index: Integer; Compare: TListSortCompare): Boolean;
var
  L, R, CompareResult: Integer;
begin
  Result := False;
  L := 0;
  R := Count - 1;
  while (not Result) and (L <= R) do
  begin
    Index := (L + R) div 2;
    CompareResult := Compare(List[Index], Item);
    if CompareResult < 0 then
      L := Index + 1
    else if CompareResult > 0 then
      R := Index - 1
    else
      Result := True;
  end; {while}
  if not Result then Index := L;
end; {function}

procedure QuickSort(List: TList; L, R: Integer; Compare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := List[(L + R) shr 1];
    repeat
      while Compare(List[I], P) < 0 do
        Inc(I);
      while Compare(List[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := List[I];
        List[I] := List[J];
        List[J] := T;
        Inc(I);
        Dec(J);
      end; {if}
    until I > J;
    if L < J then
      QuickSort(List, L, J, Compare);
    L := I;
  until I >= R;
end; {procedure}

function CompareIntegers(Item1, Item2: Pointer): Integer;
var
  Value1: Integer absolute Item1;
  Value2: Integer absolute Item2;
begin
  if Value1 < Value2 then
    Result := -1
  else if Value1 > Value2 then
    Result := 1
  else
    Result := 0;
end; {function}

{---------- TadsSortedList ----------------------------------------------------}

function TadsSortedList.AddSorted(Item: Pointer; Compare: TListSortCompare): Integer;
begin
  BinarySearch(Item, Self, Count, Result, Compare);
  if Result = Count then
    Add(Item)
  else
    Insert(Result, Item);
end; {function}

function TadsSortedList.IndexOfSorted(Item: Pointer; Compare: TListSortCompare): Integer;
begin
  if not BinarySearch(Item, Self, Count, Result, Compare) then
    Result := -1;
end; {function}

procedure TadsSortedList.Sort(Compare: TListSortCompare);
begin
  if Count > 0 then
    QuickSort(Self, 0, Count-1, Compare);
end; {procedure}

procedure TadsSortedList.Delete(Index: Integer; DeleteObject: Boolean);
begin
  if DeleteObject then
    TObject(Items[Index]).Free;
  inherited Delete(Index);
end; {procedure}

procedure TadsSortedList.ClearObjects;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    TObject(Items[i]).Free;
  inherited Clear;
end; {procedure}

{---------- TadsIntegerList ---------------------------------------------------}

function TadsIntegerList.GetItems(Index: Integer): Integer;
begin
  Result := Integer(Get(Index));
end; {function}

procedure TadsIntegerList.SetItems(Index: Integer; const Value: Integer);
begin
  Put(Index, Pointer(Value));
end; {procedure}

function TadsIntegerList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(Pointer(Item));
end; {function}

function TadsIntegerList.AddSorted(Item: Integer): Integer;
begin
  Result := inherited AddSorted(Pointer(Item), CompareIntegers);
end; {function}

function TadsIntegerList.IndexOf(Item: Integer): Integer;
begin
  Result := inherited IndexOf(Pointer(Item));
end; {function}

function TadsIntegerList.IndexOfSorted(Item: Integer): Integer;
begin
  Result := inherited IndexOfSorted(Pointer(Item), CompareIntegers);
end; {function}

function TadsIntegerList.Remove(Item: Integer): Integer;
begin
  Result := inherited Remove(Pointer(Item));
end; {function}

procedure TadsIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, Pointer(Item));
end; {procedure}

procedure TadsIntegerList.Delete(Index: Integer);
begin
  inherited Delete(Index, False);
end; {procedure}

procedure TadsIntegerList.Sort;
begin
  inherited Sort(CompareIntegers);
end; {procedure}

end.
