{** This unit contains a number of array based list classes derived from a
 common ancestor, and a doubly linked list class. In addition, the methods for
 sorting and performing binary searches used in the array based list classes are
 made public. }
unit adsListClasses;

{ Johan Lindberg 2002 (latest rev 5155)}

interface

uses Classes;

const
  MaxArrayBytes = Maxint div 4; { This is to get the same max size as TList. }
  MaxByteListSize = MaxArrayBytes;
  MaxFloatListSize = MaxArrayBytes div SizeOf(Double);
  MaxIntegerListSize = MaxArrayBytes div SizeOf(Integer);
  MaxBooleanListSize = MaxInt div 2; {Should be dividable by SizeOf(Integer) * 8}

type
  {** This is the ancestor of the array-based list classes in this unit. This
   abstract class includes the parts that are independent of the data type
   stored. The description of the public methods and properties below states
   the behaviour in a correctly implemented descendant class, not what is done
   in these methods on the base level.}
  TArrayBasedList = class
  protected
    FCapacity: Integer;
    FCount: Integer;
    function FirstGrowCapacity: Integer; virtual; abstract;
    function MaxCapacity: Integer; virtual; abstract;
    procedure AssignError;
    procedure CheckBounds(Index: Integer);
    procedure CheckEmpty;
    procedure Grow; virtual;
    procedure Reallocate(NewCapacity: Integer); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetCount(NewCount: Integer); virtual;
    procedure SizeError;
  public

    {** Checks if the list contents is identical to the contents of OtherList.
     @returns True if the lists have the same Count and all items are the same,
      and False otherwise. The check is interrupted as soon as a non-identical
      item is detected.}
    function IdenticalTo(OtherList: TArrayBasedList): Boolean; virtual;

    {** Returns True if Index is within list bounds
    (i.e. 0 <= Index <= Count - 1), and False otherwise.}
    function ItemExists(Index: Integer): Boolean;

    {** Appends the items in OtherList to the end of this list. The new Count
     will thus be equal to the old Count plus OtherList.Count.
     @raises EListError if OtherList is not of the same type as this list.}
    procedure Append(OtherList: TArrayBasedList); virtual; 

    {** Makes the list a copy of OtherList with same Count and items.
     @raises EListError if OtherList is not of the same type as this list.}
    procedure Assign(OtherList: TArrayBasedList); virtual;

    {** Releases all memory for the array and sets Count to 0.}
    procedure Clear;

    {** Switches the items in positions Index1 and Index2.
     @raises EListError if Index1 or Index2 is out of list bounds.}
    procedure Exchange(Index1, Index2: Integer); virtual;

    {** Move the item in position CurIndex to the position NewIndex. All items
     between CurIndex and NewIndex are shifted one step, so this operation takes
     time proportional to Count in the worst case.
     @raises EListError if CurIndex or NewIndex is out of list bounds.}
    procedure Move(CurIndex, NewIndex: Integer); virtual;

    {** Reverses the order of the items in the list. For example, the item on
     position i in the list will move to position Count - 1 - i, and the other
     way around.}
    procedure ReverseOrder; virtual; abstract; 

    {** The maximum number of items the list can hold without having to resize
     the underlying array. The memory usage of the list (except for TBooleanList)
     is 12 + Capacity * S bytes in Win32, where S is the size of the element
     type stored (e.g. S = 4 for an Integer). If you know in advance how many
     items you will add to a list, setting Capacity to this number before adding
     will make the adding process as efficient as possible.}
    property Capacity: Integer read FCapacity write SetCapacity;

    {** The number of items currently stored in the list. If you increase Count
     manually, the values of the thus introduced items may be undefined. If
     needed, Capacity is increased automatically when Count is increased.}
    property Count: Integer read FCount write SetCount;

    destructor Destroy; override;
  end {class};

  TBit = 0..SizeOf(Integer) * 8 - 1;
  TBits = set of TBit;
  PBoolArray = ^TBoolArray;
  TBoolArray = array[0..MaxIntegerListSize - 1] of TBits;

  {** This class maintains a list of boolean values. The items are stored in
   single bits, so the class is very memory efficient. A consequence of this is
   that Capacity must be a multiple of the number of bits in an integer (32 for
   Win32). The methods that work on large parts of the list (i.e. Assign, Insert
   and Delete) are very efficient since many bits can be handled in a single
   operation, although the asymptotic time bound is still of course the same.
   The class has only a subset of the methods of the other TList-like classes,
   since some methods are not very meaningful for boolean values. Note that the
   Items property were called Values in older versions of this class. For the
   methods without description in this class, you are referred
   to <see Class=TArrayBasedList>.}
  TBooleanList = class(TArrayBasedList)
  private
    function GetItems(Index: Integer): Boolean;
    procedure SetItems(Index: Integer; Value: Boolean);
  protected
    FList: PBoolArray;
    function GetBit(Index: Integer): Boolean;
    function FirstGrowCapacity: Integer; override;
    function MaxCapacity: Integer; override;
    procedure Reallocate(NewCapacity: Integer); override;
    procedure SetBit(Index: Integer; Value: Boolean);
    procedure SetCapacity(Value: Integer); override;
    procedure SetCount(Value: Integer); override;
  public

    {** Adds the specified value to the end of the list. If needed, the
     underlying array will be resized automatically to fit the new item. When
     this happens the operation may take time proportional to Count, but the
     average time of this operation is still constant since Capacity is doubled
     for each resize.
     @returns the index of the newly added value.}
    function Add(Value: Boolean): Integer;

    {** Returns the value of the first item in the list.
     @raises EListError if there are no items in the list.}
    function First: Boolean;

    function IdenticalTo(OtherList: TArrayBasedList): Boolean; override;

    {** Returns the value of the last item in the list.
     @raises EListError if there are no items in the list.}
    function Last: Boolean;

    procedure Append(OtherList: TArrayBasedList); override; 
    procedure Assign(OtherList: TArrayBasedList); override;

    {** Deletes the item in position Index. Note that this operation takes time
     proportional to Count in the worst case.
     @raises EListError if Index is out of list bounds.}
    procedure Delete(Index: Integer);

    procedure Exchange(Index1, Index2: Integer); override;

    {** Inserts an item with the specified value at position Index. The items
     from position Index and forward is shifted one step, so this operation
     takes time proportional to Count in the worst case. It is allowed to make a
     call with Index = Count, this translates to an Add call.
     @raises EListError if Index is < 0 or > Count.}
    procedure Insert(Index: Integer; Value: Boolean);

    procedure Move(CurIndex, NewIndex: Integer); override;

    procedure ReverseOrder; override; 

    {** Sets all items to the specified value.}
    procedure SetAll(Value: Boolean);

    {** Gets or sets the value of the item in position Index. This is the
     default array property of the class, i.e. writing VariableName[Index] is
     equivalent to VariableName.Items[Index].
     @raises EListError if Index is out of list bounds.}
    property Items[Index: Integer]: Boolean read GetItems write SetItems; default;

  end {class};

  TByteArray = array[0..MaxByteListSize - 1] of Byte;
  PByteArray = ^TByteArray;

  {** This class maintains an array-based list of bytes. It can be used as a
   memory efficient alternative to TIntegerList if all items stored are in the
   range [0..255], or for storing general binary data. For the methods
   without description in this class, you are referred to
   <see Class=TArrayBasedList>.}
  TByteList = class(TArrayBasedList)
  private
    FList: PByteArray;
    function GetItems(Index: Integer): Byte;
    procedure SetItems(Index: Integer; const Value: Byte);
  protected
    function FirstGrowCapacity: Integer; override;
    function MaxCapacity: Integer; override;
    procedure Reallocate(NewCapacity: Integer); override;
  public

    {** Adds the specified item to the end of the list. If needed, the
     underlying array will be resized automatically to fit the new item. When
     this happens the operation may take time proportional to Count, but the
     average time of this operation is still constant since Capacity is doubled
     for each resize.
     @returns the index of the newly added item.}
    function Add(Item: Byte): Integer;

    {** Returns the first item in the list.
     @raises EListError if there are no items in the list.}
    function First: Byte;

    function IdenticalTo(OtherList: TArrayBasedList): Boolean; override;

    {** Returns the index of (the first occurence of) the specified item in the
     list, or -1 if it is not in the list. This operation takes time
     proportional to Count in the worst case.}
    function IndexOf(Item: Byte): Integer;

    {** Returns the last item in the list.
     @raises EListError if there are no items in the list.}
    function Last: Byte;

    {** Deletes (the first occurence of) the specified item in the list.
     @returns the index the item had before deletion, or -1 if it did not
     exist in the list.}
    function Remove(Item: Byte): Integer;

    procedure Append(OtherList: TArrayBasedList); override; 
    procedure Assign(OtherList: TArrayBasedList); override;

    {** Deletes the item in position Index. Note that this operation takes time
     proportional to Count in the worst case.
     @raises EListError if Index is out of list bounds.}
    procedure Delete(Index: Integer);

    procedure Exchange(Index1, Index2: Integer); override;

    {** Inserts the specified item at position Index. The items from position
     Index and forward is shifted one step, so this operation takes time
     proportional to Count in the worst case. It is allowed to make a call with
     Index = Count, this translates to an Add call.
     @raises EListError if Index is < 0 or > Count.}
    procedure Insert(Index: Integer; Item: Byte);

    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure ReverseOrder; override;

    {** Sets all items to the specified value.}
    procedure SetAll(Value: Byte);

    {** Gets or sets the item in position Index. This is the default array
     property of the class, i.e. writing VariableName[Index] is equivalent to
     VariableName.Items[Index].
     @raises EListError if Index is out of list bounds.}
    property Items[Index: Integer]: Byte read GetItems write SetItems; default;

    {** Gives you direct access to the underlying array. You are advised to use
     List instead of Items only if high performance is vital, and you must then
     check yourself that you index the array only in the range [0..Count - 1].}
    property List: PByteArray read FList;

  end {class};

  {** Method type used by TEnhancedList.Iterate and TDoubleLinkList.Iterate for
   each traversed item.
   @param Item The pointer value of the current item
   @param Continue Set this to False if you want to stop the iteration.
    Continue is by default True, so if you want to iterate all the way to the
    beginning/end of the list you do not need to worry about setting Continue.}
  TIterateMethod = procedure(var Item: Pointer; var Continue: Boolean) of object;

  {** Action to take with a pointer item before deleting it.
   daDoNothing means that no action is taken,
   daFreeObject means that TObject(Item).Free will be performed and will thus
    only work if the item points to an instance of a class with properly
    overridden destructor or if Item is nil.
   daFreeMem means that FreeMem(Item) will be called if Item <> nil.}
  TDeleteAction = (daDoNothing, daFreeObject, daFreeMem);

  {** This is a replacement of the standard TList class that has a number of
   additional methods and also is faster in some cases. Although using
   TArrayBasedList as the base class introduces some overhead in some methods in
   order to avoid code duplication, this is in most cases more than compensated
   by removing some overhead TList has introduced in order to facilitate certain
   functionality in possible descendants, a feature you are in most cases not
   interested in. For example, calling Clear and setting the Count property can
   be significantly faster in this class. This class has the same properties and
   methods as the other TList-like classes, plus an Iterate method and some
   methods for keeping the list sorted. The latter enhancements makes the old
   Enera class TSortableList obsolete. For the methods without description in
   this class, you are referred to <see Class=TArrayBasedList>.}
  TEnhancedList = class(TArrayBasedList)
  private
    FList: PPointerList;
    function GetItems(Index: Integer): Pointer;
    procedure SetItems(Index: Integer; const Value: Pointer);
  protected
    function FirstGrowCapacity: Integer; override;
    function MaxCapacity: Integer; override;
    procedure Reallocate(NewCapacity: Integer); override;
  public

    {** Adds the specified item to the end of the list. If needed, the
     underlying array will be resized automatically to fit the new item. When
     this happens the operation may take time proportional to Count, but the
     average time of this operation is still constant since Capacity is doubled
     for each resize.
     @returns the index of the newly added item.}
    function Add(Item: Pointer): Integer;

    {** Adds the specified item to the list in sorted order. This method takes
     time proportional to Count in the worst case, so if you are sequentially
     adding a large number of items to the list, it is faster to use the normal
     Add function instead and then call Sort when you have added all items.
     @param Item The item to add.
     @param Compare is a function that compares the data that two pointers Item1
      and Item 2 references and returns < 0 if Item1's data is less than Item2's,
      0 if they are equal and > 0 if Item1's data is greater than Item2's.
     @returns the index the item was inserted on.}
    function AddSorted(Item: Pointer; Compare: TListSortCompare): Integer;

    {** Returns the first item in the list.
     @raises EListError if there are no items in the list.}
    function First: Pointer;

    function IdenticalTo(OtherList: TArrayBasedList): Boolean; override;

    {** Returns the index of (the first occurence of) the specified item in the
     list, or -1 if it is not in the list. This operation takes time
     proportional to Count in the worst case.}
    function IndexOf(Item: Pointer): Integer;

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

    {** Iterates through a number of list items until outside list bounds or
     the user stops the iteration. For each item visited, a user-defined method
     is called. Useful e.g. for searching.
     Warning: you should not call any methods or set any properties of
     TEnhancedList while in the user-defined method.
     @param StartIndex The starting point of the iteration.
     @param IterateForward True if index should be increased during iteration,
      and False if it should be decreased.
     @param IterateMethod The user-defined method called for each item.
     @returns The index the iteration was stopped on, or -1 if not stopped.
     @raises EListError if IterateMethod is not assigned when Iterate is called,
      or if .}
    function Iterate(StartIndex: Integer; IterateForward: Boolean; const
      IterateMethod: TIterateMethod): Integer;

    {** Returns the last item in the list.
     @raises EListError if there are no items in the list.}
    function Last: Pointer;

    {** Deletes (the first occurence of) the specified item in the list.
     @param DeleteAction This optional parameter can be used if you want to free
      up memory associated to the item.
     @returns the index the item had before deletion, or -1 if it did not
     exist in the list.}
    function Remove(Item: Pointer; DeleteAction: TDeleteAction = daDoNothing):
      Integer; 

    procedure Append(OtherList: TArrayBasedList); override; 
    procedure Assign(OtherList: TArrayBasedList); override;

    {** Releases all memory for the array and sets Count to 0.
     @param DeleteAction This optional parameter can be used if you want to free
      up memory associated to the items in the list.}
    procedure Clear(DeleteAction: TDeleteAction = daDoNothing); 

    {** Deletes the item in position Index. Note that this operation takes time
     proportional to Count in the worst case.
     @param DeleteAction This optional parameter can be used if you want to free
      up memory associated to the item.
     @raises EListError if Index is out of list bounds.}
    procedure Delete(Index: Integer; DeleteAction: TDeleteAction = daDoNothing);

    procedure Exchange(Index1, Index2: Integer); override;

    {** If the items in the list are all descendants of TObject with properly
     overridden Destroy methods, you can use this method to free all objects and
     clear the list. This procedure works also if some items are nil.
     This obsolete method is equivalent to Clear(daFreeObject), and may be
     removed in the future.}
    procedure FreeObjectsAndClear; 

    {** Inserts the specified item at position Index. The items from position
     Index and forward is shifted one step, so this operation takes time
     proportional to Count in the worst case. It is allowed to make a call with
     Index = Count, this translates to an Add call.
     @raises EListError if Index is < 0 or > Count.}
    procedure Insert(Index: Integer; Item: Pointer);

    procedure Move(CurIndex, NewIndex: Integer); override;

    {** Removes all nil items from the list.}
    procedure Pack;

    procedure ReverseOrder; override;

    {** Sets all items to the specified value.}
    procedure SetAll(Value: Pointer);

    {** Sorts the items in the list in ascending order using a QuickSort
     algorithm. This operation takes time proportional to Count * log(Count) on
     average. Does nothing if the list is empty.
     @param Compare is a function that compares the data that two pointers Item1
      and Item 2 references and returns < 0 if Item1's data is less than Item2's,
      0 if they are equal and > 0 if Item1's data is greater than Item2's.}
    procedure Sort(Compare: TListSortCompare);

    {** Gets or sets the item in position Index. This is the default array
     property of the class, i.e. writing VariableName[Index] is equivalent to
     VariableName.Items[Index].
     @raises EListError if Index is out of list bounds.}
    property Items[Index: Integer]: Pointer read GetItems write SetItems; default;

    {** Gives you direct access to the underlying array. You are adviced to use
     List instead of Items only if high performance is vital, and you must then
     check yourself that you index the array only in the range [0..Count - 1].}
    property List: PPointerList read FList;

  end {class};

  TFloatArray = array[0..MaxFloatListSize - 1] of Double;
  PFloatArray = ^TFloatArray;

  {** This class maintains an array-based list of floating point numbers (of
   type Double). It has the same properties and methods as the other TList-like
   classes, plus some statistical methods and some methods for keeping the list
   sorted. For the methods without description in this class, you are referred
   to <see Class=TArrayBasedList>.}
  TFloatList = class(TArrayBasedList)
  private
    FList: PFloatArray;
    function GetItems(Index: Integer): Double;
    procedure SetItems(Index: Integer; const Value: Double);
  protected
    function FirstGrowCapacity: Integer; override;
    function MaxCapacity: Integer; override;
    procedure Reallocate(NewCapacity: Integer); override;
  public

    {** Adds the specified item to the end of the list. If needed, the
     underlying array will be resized automatically to fit the new item. When
     this happens the operation may take time proportional to Count, but the
     average time of this operation is still constant since Capacity is doubled
     for each resize.
     @returns the index of the newly added item.}
    function Add(Item: Double): Integer;

    {** Adds the specified Item in the list in sorted order. This method takes
     time proportional to Count in the worst case, so if you are sequentially
     adding a large number of items to the list, it is faster to use the normal
     Add function instead and then call Sort when you have added all items.
    @returns the index the item was inserted on.}
    function AddSorted(Item: Double): Integer;

    {** Returns the first item in the list.
     @raises EListError if there are no items in the list.}
    function First: Double;

    function IdenticalTo(OtherList: TArrayBasedList): Boolean; override;

    {** Returns the index of (the first occurence of) the specified item in the
     list, or -1 if it is not in the list. This operation takes time
     proportional to Count in the worst case.}
    function IndexOf(Item: Double): Integer;

    {** Returns the index of the specified item in the list, assuming the list
     is sorted in ascending order, or -1 if it is not in the list. Using this
     function is quicker (takes time proportional to log(Count)) than IndexOf,
     but it can only be used if the items in the list are sorted.}
    function IndexOfSorted(Item: Double): Integer;

    {** Returns the last item in the list.
     @raises EListError if there are no items in the list.}
    function Last: Double;

    {** Returns the arithmetic average of the items in the list.
     @raises EListError if there are no items in the list.}
    function Mean: Extended;

    {** Deletes (the first occurence of) the specified item in the list.
     @returns the index the item had before deletion, or -1 if it did not
     exist in the list.}
    function Remove(Item: Double): Integer;

    {** Returns the standard deviation of the items in the list.
     @raises EListError if there are no items in the list.}
    function StdDev: Extended;

    {** Returns the sum of the items in the list (0 if no items)}
    function Sum: Extended;

    procedure Append(OtherList: TArrayBasedList); override; 
    procedure Assign(OtherList: TArrayBasedList); override;

    {** Deletes the item in position Index. Note that this operation takes time
     proportional to Count in the worst case.
     @raises EListError if Index is out of list bounds.}
    procedure Delete(Index: Integer);

    procedure Exchange(Index1, Index2: Integer); override;

    {** Inserts the specified item at position Index. The items from position
     Index and forward is shifted one step, so this operation takes time
     proportional to Count in the worst case. It is allowed to make a call with
     Index = Count, this translates to an Add call.
     @raises EListError if Index is < 0 or > Count.}
    procedure Insert(Index: Integer; Item: Double);

    {** Calculates the arithmetic average and standard deviation of the items in
     the list. Using this method is faster than calling Mean and StdDev
     separately when you are interested in both.
     @raises EListError if there are no items in the list.}
    procedure MeanAndStdDev(var Mean, StdDev: Extended);

    procedure Move(CurIndex, NewIndex: Integer); override;

    procedure ReverseOrder; override; 

    {** Sets all items to the specified value.}
    procedure SetAll(Value: Double);

    {** Sorts the items in the list in ascending order using a QuickSort
     algorithm. This operation takes time proportional to Count * log(Count) on
     average. Does nothing if the list is empty.}
    procedure Sort;

    {** Gets or sets the item in position Index. This is the default array
     property of the class, i.e. writing VariableName[Index] is equivalent to
     VariableName.Items[Index].
     @raises EListError if Index is out of list bounds.}
    property Items[Index: Integer]: Double read GetItems write SetItems; default;

    {** Gives you direct access to the underlying array. You are adviced to use
     List instead of Items only if high performance is vital, and you must then
     check yourself that you index the array only in the range [0..Count - 1].}
    property List: PFloatArray read FList;

  end {class};

  TIntegerArray = array[0..MaxIntegerListSize - 1] of Integer;
  PIntegerArray = ^TIntegerArray;

  {** This class maintains an array-based list of integers. It has the same
   properties and methods as the other TList-like classes, plus a Sum function
   and some additional methods for keeping a sorted list. For the methods
   without description in this class, you are referred
   to <see Class=TArrayBasedList>.}
  TIntegerList = class(TArrayBasedList)
  private
    FList: PIntegerArray;
    function GetItems(Index: Integer): Integer;
    procedure SetItems(Index: Integer; const Value: Integer);
  protected
    function FirstGrowCapacity: Integer; override;
    function MaxCapacity: Integer; override;
    procedure Reallocate(NewCapacity: Integer); override;
  public

    {** Adds the specified item to the end of the list. If needed, the
     underlying array will be resized automatically to fit the new item. When
     this happens the operation may take time proportional to Count, but the
     average time of this operation is still constant since Capacity is doubled
     for each resize.
     @returns the index of the newly added item.}
    function Add(Item: Integer): Integer;

    {** Adds the specified Item in the list in sorted order. This method takes
     time proportional to Count in the worst case, so if you are sequentially
     adding a large number of items to the list, it is faster to use the normal
     Add function instead and then call Sort when you have added all items.
    @returns the index the item was inserted on.}
    function AddSorted(Item: Integer): Integer;

    {** Returns the first item in the list.
     @raises EListError if there are no items in the list.}
    function First: Integer;

    function IdenticalTo(OtherList: TArrayBasedList): Boolean; override;

    {** Returns the index of (the first occurence of) the specified item in the
     list, or -1 if it is not in the list. This operation takes time
     proportional to Count in the worst case.}
    function IndexOf(Item: Integer): Integer;

    {** Returns the index of the specified item in the list, assuming the list
     is sorted in ascending order, or -1 if it is not in the list. Using this
     function is quicker (takes time proportional to log(Count)) than IndexOf,
     but it can only be used if the items in the list are sorted.}
    function IndexOfSorted(Item: Integer): Integer;

    {** Returns the last item in the list.
     @raises EListError if there are no items in the list.}
    function Last: Integer;

    {** Deletes (the first occurence of) the specified item in the list.
     @returns the index the item had before deletion, or -1 if it did not
     exist in the list.}
    function Remove(Item: Integer): Integer;

    {** Returns the sum of the items in the list (0 if no items)}
    function Sum: Integer;

    procedure Append(OtherList: TArrayBasedList); override; 
    procedure Assign(OtherList: TArrayBasedList); override;

    {** Deletes the item in position Index. Note that this operation takes time
     proportional to Count in the worst case.
     @raises EListError if Index is out of list bounds.}
    procedure Delete(Index: Integer);

    procedure Exchange(Index1, Index2: Integer); override;

    {** Inserts the specified item at position Index. The items from position
     Index and forward is shifted one step, so this operation takes time
     proportional to Count in the worst case. It is allowed to make a call with
     Index = Count, this translates to an Add call.
     @raises EListError if Index is < 0 or > Count.}
    procedure Insert(Index: Integer; Item: Integer);

    procedure Move(CurIndex, NewIndex: Integer); override;

    procedure ReverseOrder; override; 

    {** Sets all items to the specified value.}
    procedure SetAll(Value: Integer);

    {** Sorts the items in the list in ascending order using a QuickSort
     algorithm. This operation takes time proportional to Count * log(Count) on
     average. Does nothing if the list is empty.}
    procedure Sort;

    {** Gets or sets the item in position Index. This is the default array
     property of the class, i.e. writing VariableName[Index] is equivalent to
     VariableName.Items[Index].
     @raises EListError if Index is out of list bounds.}
    property Items[Index: Integer]: Integer read GetItems write SetItems; default;

    {** Gives you direct access to the underlying array. You are adviced to use
     List instead of Items only if high performance is vital, and you must then
     check yourself that you index the array only in the range [0..Count - 1].}
    property List: PIntegerArray read FList;

  end {class};

  {Record types for internal use in TDoubleLinkList}
  PListRecord = ^TListRecord;
  TListRecord = record
    Item: Pointer;
    Next: PListRecord;
    Previous: PListRecord;
  end {record};

  {** The possible alternatives for where to insert items in
   the <see class=TDoubleLinkList>.}
  TInsertPosition = (ipAtBeginning, ipAtEnd, ipBeforeCurrent, ipAfterCurrent);

  {** The possible start point and direction alternatives for the Iterate
   method of <see class=TDoubleLinkList>.}
  TIterateAlternative = (iaForwardFromBeginning, iaForwardFromCurrent,
    iaBackwardFromCurrent, iaBackwardFromEnd);

  {** This class maintains a doubly linked list of pointer items that you can
   use for storing any kind of data (by reference). Use this class instead of
   TList or TEnhancedList if efficiency of insertion and deletion operations is
   more important than efficiency of random access to list elements and memory
   usage. This class uses 40 + 12 * N bytes of memory for N list items in Win32
   (excluding the data referenced by the items), whereas TList and TEnhancedList
   uses at minimum 16 + 4 * N bytes (if their Capacity is not > N).

   TDoubleLinkList has an Items array property like TList, but you can also
   navigate in the list much like with a TTable by using the First, Last, Next
   and Prior methods and reading the CurrentItem, CurrentIndex and EndOfList
   properties. In addition, you can use the method Iterate to let
   TDoubleLinkList handle the list traversing for you. The class also has a
   number of methods for inserting, deleting and finding items.}
  TDoubleLinkList = class
  private
    function GetCurrentItem: Pointer;
    function GetEndOfList: Boolean;
    function GetItems(Index: Integer): Pointer;
    procedure SetCurrentIndex(const Value: Integer);
    procedure SetCurrentItem(const Value: Pointer);
    procedure SetItems(Index: Integer; const Value: Pointer);
  protected
    Current: PListRecord;
    FCount: Integer;
    FCurrentIndex: Integer;
    Head: PListRecord;
    Tail: PListRecord;
    NilNode: PListRecord; 
    procedure RaiseExceptionIfNoCurrent;
  public
    {** Locates the specified item in the list and makes it the current one. If
     no such item exists, there will be no current record in the list. Search
     is started at the beginning of the list and stopped at the first occurrence
     of Item. If a different search behaviour is wanted, use the Iterate method
     instead or navigate manually in the list.
     @returns True if the item is located, False otherwise.}
    function Find(Item: Pointer): Boolean;

    {** Deletes all items from the list.
     @param DeleteAction This optional parameter can be used if you want to free
      up memory associated to the items.}
    procedure Clear(DeleteAction: TDeleteAction = daDoNothing); 

    {** Deletes the current item from the list. If the last item in the list is
     deleted, the item before this will be the next current one (if there are
     no items left after deletion, there will of course be no current record).
     In all other cases, the item after the deleted one will be the new current
     item.
     @param DeleteAction This optional parameter can be used if you want to free
      up memory associated to the item.
     @raises EListError if called when there is no current record.}
    procedure Delete(DeleteAction: TDeleteAction = daDoNothing);

    {** Makes the first item in the list the current one. Has no effect if the
     list is empty.}
    procedure First;

    {** Inserts a new item into the list and makes it the current one. The
     place where the item is inserted depends on the Position parameter. If
     Position is left out, it will by default have the ipBeforeCurrent value.
     An EListError exception will be raised if called with Position =
     ipBeforeCurrent or ipAfterCurrent if there is no current record.}
    procedure Insert(Item: Pointer; Position: TInsertPosition =
      ipBeforeCurrent);

    {** Iterates through a number of list items until outside list bounds or
     the user stops the iteration. For each item visited, a user-defined method
     is called. When the method finishes, the current item will be the one where
     the user stopped the iteration or none if the iteration was not stopped.
     Using Iterate is slightly quicker than traversing the list manually.
     Warning: you should not call any methods or set any properties of
     TDoubleLinkList while in the user-defined method.
     @param Alternative Specifies the starting point and direction of iteration.
     @param IterateMethod The user-defined method called for each item.
     @raises EListError if IterateMethod is not assigned when Iterate is called,
      or if there is no current item when Alternative is iaForwardFromCurrent
      or iaBackWardFromCurrent.}
    procedure Iterate(Alternative: TIterateAlternative; const IterateMethod:
      TIterateMethod);

    {** Makes the last item in the list the current one. Has no effect if the
     list is empty.}
    procedure Last;

    {** Makes the item after the current item in the list the new current one.
     If the current item was the last in the list, there will no longer be any
     current item in the list and the EndOfList property will flag True.
     @raises EListError if called when there is no current item.}
    procedure Next;

    {** Makes the item before the current item in the list the new current one.
     If the current item was the first in the list, there will no longer be
     any current item in the list and the EndOfList property will flag True.
     @raises EListError if called when there is no current item.}
    procedure Prior;

    {** This read-only property returns the number of items in the list.}
    property Count: Integer read FCount;

    {** The index of the current item in the list (zero-based). If there is no
     current item (happens if there are no items in the list, or if one has
     navigated out of list bounds with Next, Prior or Find), CurrentIndex will
     return -1 when read. When set, the item on the specified position is set as
     the current in the list. Finding the item in question will be quick if the
     new CurrentIndex value is near the old one or one of the list ends,
     otherwise it will be slower since a large number of list elements may have
     to be traversed to reach the item.
     @raises EListError if an out-of-bounds index is attempted to be set.}
    property CurrentIndex: Integer read FCurrentIndex write SetCurrentIndex;

    {** The pointer value of the current item in the list. If there is no
     current item (happens if there are no items in the list, or if one has
     navigated out of list bounds with Next, Prior or Find), CurrentItem will
     return nil when read. When set, the pointer value of the current item will
     change.
     @raises EListError if trying to set the CurrentItem pointer value when
     there is no current item.}
    property CurrentItem: Pointer read GetCurrentItem write SetCurrentItem;

    {** This read-only property returns True if there is no current item in the
     list (happens if there are no items in the list, or if Next is called when
     the last item in the list is current, or when Prior is called when the
     first item in the list is current, or when Find does not successfully
     locate an item), and False otherwise.}
    property EndOfList: Boolean read GetEndOfList;

    {** Makes the item in position Index the current item in the list and gets
     or sets its pointer value. Finding the item in question will be quick if
     Index is near the old CurrentIndex or one of the list ends, otherwise it
     will be slower since a large number of list elements may have to be
     traversed to reach the item.
     @raises EListError if Index is out of list bounds.}
    property Items[Index: Integer]: Pointer read GetItems write SetItems;

    constructor Create;
    destructor Destroy; override;
  end {class};

  {** This comparison function can be used with Sort, IndexOfSorted and
   AddSorted of TEnhancedList or with other sorted data structures (for
   example TBinarySearchTree and TSplayTree) when you want to locate items by
   their pointer value only (for example when assuring that you do not have any
   duplicate items).}
  function CompareAsInteger(Item1, Item2: Pointer): Integer; 

  {** Searches for the position of an item in a sorted array of floating point
   numbers.
   @param Item The value to search for.
   @param List Pointer to the array.
   @param Count The number of elements in the array.
   @param Index If the item is found, Index holds its position in the array.
    Otherwise Index points at the position where the item would be if inserted.
   @returns True if the item is found and False otherwise.}
  function BinarySearchFloat(const Item: Double; List: PFloatArray; const Count:
    Integer; var Index: Integer): Boolean;

  {** Searches for the position of an item in a sorted array of integers.
   @param Item The value to search for.
   @param List Pointer to the array.
   @param Count The number of elements in the array.
   @param Index If the item is found, Index holds its position in the array.
    Otherwise Index points at the position where the item would be if inserted.
   @returns True if the item is found and False otherwise.}
  function BinarySearchInteger(const Item: Integer; List: PIntegerArray; const
    Count: Integer; var Index: Integer): Boolean;

  {** Searches for the position of an item in an array of pointers, sorted with
   respect to the data they reference.
   @param Item An item that has the data content you are searching for.
   @param List Pointer to the array.
   @param Count The number of elements in the array.
   @param Index If the item is found, Index holds its position in the array.
    Otherwise Index points at the position where the item would be if inserted.
   @returns True if the item is found and False otherwise.
   @param SCompare is a function that compares the data that two pointers Item1
    and Item 2 references and returns < 0 if Item1's data is less than Item2's,
    0 if they are equal and > 0 if Item1's data is greater than Item2's.}
  function BinarySearchPointer(Item: Pointer; List: PPointerList; const Count:
    Integer; var Index: Integer; SCompare: TListSortCompare): Boolean;

  {** Sorts (a part of) an array of floating point numbers in ascending order.
   @param List Pointer to the array.
   @param L The leftmost index of the items you want sorted (typically = 0)
   @param R The leftmost index of the items you want sorted (typically = N - 1,
    where N is the number of elements in the array)}
  procedure QuickSortFloat(List: PFloatArray; L, R: Integer);

  {** Sorts (a part of) an array of integers in ascending order.
   @param List Pointer to the array.
   @param L The leftmost index of the items you want sorted (typically = 0)
   @param R The leftmost index of the items you want sorted (typically = N - 1,
    where N is the number of elements in the array)}
  procedure QuickSortInteger(List: PIntegerArray; L, R: Integer);

  {** Sorts (a part of) an array of pointers in ascending order with regards to
   the data the pointers reference.
   @param List Pointer to the array.
   @param L The leftmost index of the items you want sorted (typically = 0)
   @param R The leftmost index of the items you want sorted (typically = N - 1,
    where N is the number of elements in the array)
   @param SCompare is a function that compares the data that two pointers Item1
    and Item 2 references and returns < 0 if Item1's data is less than Item2's,
    0 if they are equal and > 0 if Item1's data is greater than Item2's.}
  procedure QuickSortPointer(List: PPointerList; L, R: Integer;
    SCompare: TListSortCompare);

  {** Performs the action described in the TDeleteAction type}
  procedure PerformDeleteAction(var Item: Pointer; Action: TDeleteAction);

implementation

uses
  SysUtils, Math;

function CompareAsInteger(Item1, Item2: Pointer): Integer; 
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
end {function};

function BinarySearchFloat(const Item: Double; List: PFloatArray; const Count:
  Integer; var Index: Integer): Boolean;
var
  L, R: Integer;
begin
  Result := False;
  L := 0;
  R := Count - 1;
  while (not Result) and (L <= R) do
  begin
    Index := (L + R) div 2;
    if List^[Index] < Item then
      L := Index + 1
    else if List^[Index] > Item then
      R := Index - 1
    else
      Result := True;
  end {while};
  if not Result then Index := L;
end {function};

function BinarySearchInteger(const Item: Integer; List: PIntegerArray; const
  Count: Integer; var Index: Integer): Boolean;
var
  L, R: Integer;
begin
  Result := False;
  L := 0;
  R := Count - 1;
  while (not Result) and (L <= R) do
  begin
    Index := (L + R) div 2;
    if List^[Index] < Item then
      L := Index + 1
    else if List^[Index] > Item then
      R := Index - 1
    else
      Result := True;
  end {while};
  if not Result then Index := L;
end {function};

function BinarySearchPointer(Item: Pointer; List: PPointerList; const Count:
  Integer; var Index: Integer; SCompare: TListSortCompare): Boolean;
var
  L, R, CompareResult: Integer;
begin
  Result := False;
  L := 0;
  R := Count - 1;
  while (not Result) and (L <= R) do
  begin
    Index := (L + R) div 2;
    CompareResult := SCompare(List^[Index], Item);
    if CompareResult < 0 then
      L := Index + 1
    else if CompareResult > 0 then
      R := Index - 1
    else
      Result := True;
  end {while};
  if not Result then Index := L;
end {function};

procedure QuickSortFloat(List: PFloatArray; L, R: Integer);
var
  I, J: Integer;
  P, T: Double;
begin
  repeat
    I := L;
    J := R;
    P := List^[(L + R) shr 1];
    repeat
      while List^[I] < P do
        Inc(I);
      while List^[J] > P do
        Dec(J);
      if I <= J then
      begin
        T := List^[I];
        List^[I] := List^[J];
        List^[J] := T;
        Inc(I);
        Dec(J);
      end {if};
    until I > J;
    if L < J then
      QuickSortFloat(List, L, J);
    L := I;
  until I >= R;
end {procedure};

procedure QuickSortInteger(List: PIntegerArray; L, R: Integer);
var
  I, J, P, T: Integer;
begin
  repeat
    I := L;
    J := R;
    P := List^[(L + R) shr 1];
    repeat
      while List^[I] < P do
        Inc(I);
      while List^[J] > P do
        Dec(J);
      if I <= J then
      begin
        T := List^[I];
        List^[I] := List^[J];
        List^[J] := T;
        Inc(I);
        Dec(J);
      end {if};
    until I > J;
    if L < J then
      QuickSortInteger(List, L, J);
    L := I;
  until I >= R;
end {procedure};

procedure QuickSortPointer(List: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := List^[(L + R) shr 1];
    repeat
      while SCompare(List^[I], P) < 0 do
        Inc(I);
      while SCompare(List^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := List^[I];
        List^[I] := List^[J];
        List^[J] := T;
        Inc(I);
        Dec(J);
      end {if};
    until I > J;
    if L < J then
      QuickSortPointer(List, L, J, SCompare);
    L := I;
  until I >= R;
end {procedure};

procedure PerformDeleteAction(var Item: Pointer; Action: TDeleteAction);
begin
  case Action of
    daFreeObject: TObject(Item).Free;
    daFreeMem: if Item <> nil then FreeMem(Item);
  end {case};
end {procedure};

{ TArrayBasedList }

procedure TArrayBasedList.AssignError;
begin
  raise EListError.Create('Assign of different list classes not possible');
end {procedure};

procedure TArrayBasedList.CheckBounds(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.Create('List index out of bounds');
end {procedure};

procedure TArrayBasedList.CheckEmpty;
begin
  if FCount = 0 then raise EListError.Create('List is empty');
end {procedure};

procedure TArrayBasedList.SizeError;
begin
  raise EListError.Create('Invalid list size');
end {procedure};

procedure TArrayBasedList.Grow;
var
  NewCapacity: Integer;
begin
  if FCapacity > 0 then
  begin
    NewCapacity := FCapacity * 2;
    if NewCapacity > MaxCapacity then
      NewCapacity := MaxCapacity;
  end {if}
  else NewCapacity := FirstGrowCapacity;
  SetCapacity(NewCapacity);
end {procedure};

procedure TArrayBasedList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxCapacity) then
    SizeError;
  if NewCapacity <> Capacity then
  begin
    Reallocate(NewCapacity);
    FCapacity := NewCapacity;
  end {if};
end {function};

procedure TArrayBasedList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxCapacity) then
    SizeError;
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  FCount := NewCount;
end {function};

function TArrayBasedList.IdenticalTo(OtherList: TArrayBasedList): Boolean;
begin
  Result := FCount = OtherList.Count;
  {The rest must be performed in descendants}
end {function};

function TArrayBasedList.ItemExists(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FCount);
end {function};

procedure TArrayBasedList.Append(OtherList: TArrayBasedList); 
begin
  SetCount(FCount + OtherList.Count);
  {The rest must be performed in descendants}
end {procedure};

procedure TArrayBasedList.Assign(OtherList: TArrayBasedList);
begin
  SetCount(OtherList.Count);
  {The rest must be performed in descendants}
end {procedure};

procedure TArrayBasedList.Clear;
begin
  FCount := 0;
  SetCapacity(0);
end {procedure};

procedure TArrayBasedList.Exchange(Index1, Index2: Integer);
begin
  CheckBounds(Index1);
  CheckBounds(Index2);
  {The rest must be performed in descendants}
end {procedure};

procedure TArrayBasedList.Move(CurIndex, NewIndex: Integer);
begin
  CheckBounds(CurIndex);
  CheckBounds(NewIndex);
  {The rest must be performed in descendants}
end {procedure};

destructor TArrayBasedList.Destroy;
begin
  Clear;
  inherited;
end {destructor};

{ TBooleanList}

const
  BitsCount = 8 * SizeOf(TBits);

function TBooleanList.GetItems(Index: Integer): Boolean;
begin
  CheckBounds(Index);
  Result := GetBit(Index);
end {function};

procedure TBooleanList.SetItems(Index: Integer; Value: Boolean);
begin
  CheckBounds(Index);
  SetBit(Index, Value);
end {procedure};

function TBooleanList.FirstGrowCapacity: Integer;
begin
  Result := BitsCount;
end {function};

function TBooleanList.GetBit(Index: Integer): Boolean;
begin
  Result := (Index mod BitsCount) in FList^[Index div BitsCount];
end {function};

function TBooleanList.MaxCapacity: Integer;
begin
  Result := MaxBooleanListSize;
end {function};

procedure TBooleanList.Reallocate(NewCapacity: Integer);
begin
  ReallocMem(FList, (NewCapacity div BitsCount) * SizeOf(TBits));
end {procedure};

procedure TBooleanList.SetBit(Index: Integer; Value: Boolean);
var
  BitsIndex: Integer;
  Bit: TBit;
begin
  BitsIndex := Index div BitsCount;
  Bit := Index mod BitsCount;
  if Value then Include(FList^[BitsIndex], Bit)
  else Exclude(FList^[BitsIndex], Bit);
end {procedure};

procedure TBooleanList.SetCapacity(Value: Integer);
begin
  if (Value mod BitsCount) = 0 then
    inherited
  else SizeError;
end {procedure};

procedure TBooleanList.SetCount(Value: Integer);
begin
  if (Value < 0) or (Value > MaxListSize) then
    SizeError;
  if Value > FCapacity then
    SetCapacity(((Value + BitsCount - 1) div BitsCount) * BitsCount);
  FCount := Value;
end {procedure};

function TBooleanList.Add(Value: Boolean): Integer;
begin
  Result := FCount;
  if FCount = FCapacity then Grow;
  SetBit(Result, Value);
  Inc(FCount);
end {function};

function TBooleanList.First: Boolean;
begin
  CheckEmpty;
  Result := GetBit(0);
end {function};

function TBooleanList.IdenticalTo(OtherList: TArrayBasedList): Boolean;
var
  i, FullTBits: Integer;
  OtherBooleanList: TBooleanList;
begin
  Result := (inherited IdenticalTo(OtherList)) and (OtherList is TBooleanList);
  if Result then
  begin
    OtherBooleanList := TBooleanList(OtherList);
    FullTBits := Count div BitsCount; {First compare entire TBits}
    for i := 0 to FullTBits - 1 do
      if FList^[i] <> OtherBooleanList.FList^[i] then
      begin
        Result := False;
        Break;
      end {if};
    if Result then {then compare single bits in the last TBits}
      for i := FullTBits * BitsCount to FCount - 1 do
        if GetBit(i) <> OtherBooleanList.GetBit(i) then
        begin
          Result := False;
          Break;
        end {if};
  end {if};
end {function};

function TBooleanList.Last: Boolean;
begin
  CheckEmpty;
  Result := GetBit(FCount - 1);
end {function};

procedure TBooleanList.Append(OtherList: TArrayBasedList); 
{Could perhaps be implemented more efficiently}
var
  i, OldCount: Integer;
  OtherBooleanList: TBooleanList;
begin
  if not (OtherList is TBooleanList) then AssignError;
  OldCount := Count;
  inherited;
  OtherBooleanList := TBooleanList(OtherList);
  for i := 0 to OtherBooleanList.Count - 1 do
    SetBit(OldCount + i, OtherBooleanList.GetBit(i));
end {procedure};

procedure TBooleanList.Assign(OtherList: TArrayBasedList);
var
  i: Integer;
  OtherBooleanList: TBooleanList;
begin
  if not (OtherList is TBooleanList) then AssignError;
  inherited;
  OtherBooleanList := TBooleanList(OtherList);
  for i := 0 to ((FCount + BitsCount - 1) div BitsCount) - 1 do
    FList^[i] := OtherBooleanList.FList^[i];
end {procedure};

procedure TBooleanList.Delete(Index: Integer);
var
  i, FirstEntireTBitsToShift, LastEntireTBitsToShift: Integer;
begin
  CheckBounds(Index);
  Dec(FCount);
(* {This is what we want to do, but faster}
  for i := Index + 1 to FCount do
    SetBit(i - 1, GetBit(i));
*)
  FirstEntireTBitsToShift := (Index + BitsCount) div BitsCount;
  for i := Index + 1 to Min(FirstEntireTBitsToShift * BitsCount, FCount) do
    SetBit(i - 1, GetBit(i));
  LastEntireTBitsToShift := (FCount + BitsCount) div BitsCount - 1;
  for i := FirstEntireTBitsToShift to LastEntireTBitsToShift do
  begin
    FList^[i] := TBits(LongWord(FList^[i]) shr 1);
    if i <> LastEntireTBitsToShift then
      SetBit(i * BitsCount + BitsCount - 1, GetBit((i + 1) * BitsCount));
  end {for};
end {procedure};

procedure TBooleanList.Exchange(Index1, Index2: Integer);
var
  Temp: Boolean;
begin
  inherited;
  Temp := GetBit(Index1);
  SetBit(Index1, GetBit(Index2));
  SetBit(Index2, Temp);
end {procedure};

procedure TBooleanList.Insert(Index: Integer; Value: Boolean);
var
  i, FirstEntireTBitsToShift, LastEntireTBitsToShift: Integer;
begin
  if Index = FCount then
    Add(Value)
  else
  begin
    CheckBounds(Index);
    if Count = Capacity then
      Grow;
  (* {This is what we want to do, but faster}
    for i := FCount downto Index + 1 do
      SetBit(i, GetBit(i - 1));
  *)
    FirstEntireTBitsToShift := (Index + BitsCount) div BitsCount;
    LastEntireTBitsToShift := (FCount + BitsCount + 1) div BitsCount - 1;
    for i := LastEntireTBitsToShift downto FirstEntireTBitsToShift do
    begin
      FList^[i] := TBits(LongWord(FList^[i]) shl 1);
      SetBit(i * BitsCount, GetBit(i * BitsCount - 1));
    end {for};
    for i := Min(FirstEntireTBitsToShift * BitsCount - 1, FCount) downto Index + 1 do
      SetBit(i, GetBit(i - 1));
    SetBit(Index, Value);
    Inc(FCount);
  end {else};  
end {procedure};

procedure TBooleanList.Move(CurIndex, NewIndex: Integer);
var
  Diff, i, FirstEntireTBitsToShift, LastEntireTBitsToShift: Integer;
  Value: Boolean;
{Could perhaps be made more efficient using bit shift operations}
begin
  inherited;
  Value := GetBit(CurIndex);
  Diff := NewIndex - CurIndex;
(* {This is what we want to do, but faster}
  if Diff > 0 then
    for i := CurIndex + 1 to NewIndex do
      SetBit(i - 1, GetBit(i))
  else if Diff < 0 then
    for i := CurIndex downto NewIndex + 1 do
      SetBit(i, GetBit(i - 1));
*)
  if Diff > 0 then
  begin
    FirstEntireTBitsToShift := (CurIndex + BitsCount) div BitsCount;
    LastEntireTBitsToShift := NewIndex div BitsCount - 1;
    if LastEntireTBitsToShift - FirstEntireTBitsToShift >= 0 then
    begin
      for i := CurIndex + 1 to FirstEntireTBitsToShift * BitsCount do
        SetBit(i - 1, GetBit(i));
      for i := FirstEntireTBitsToShift to LastEntireTBitsToShift do
      begin
        FList^[i] := TBits(LongWord(FList^[i]) shr 1);
        SetBit(i * BitsCount + BitsCount - 1, GetBit((i + 1) * BitsCount));
      end {for};
      for i := (LastEntireTBitsToShift + 1) * BitsCount + 1 to NewIndex do
        SetBit(i - 1, GetBit(i));
    end {if}
    else
      for i := CurIndex + 1 to NewIndex do
        SetBit(i - 1, GetBit(i))
  end {if}
  else if Diff < 0 then
  begin
    FirstEntireTBitsToShift := (NewIndex + BitsCount) div BitsCount;
    LastEntireTBitsToShift := CurIndex div BitsCount - 1;
    if LastEntireTBitsToShift - FirstEntireTBitsToShift >= 0 then
    begin
      for i := CurIndex downto (LastEntireTBitsToShift + 1) * BitsCount do
        SetBit(i, GetBit(i - 1));
      for i := LastEntireTBitsToShift downto FirstEntireTBitsToShift do
      begin
        FList^[i] := TBits(LongWord(FList^[i]) shl 1);
        SetBit(i * BitsCount, GetBit(i * BitsCount - 1));
      end {for};
      for i := FirstEntireTBitsToShift * BitsCount - 1 downto NewIndex + 1 do
        SetBit(i, GetBit(i - 1));
    end {if}
    else
      for i := CurIndex downto NewIndex + 1 do
        SetBit(i, GetBit(i - 1));
  end {else};
  SetBit(NewIndex, Value);
end {procedure};

procedure TBooleanList.ReverseOrder; 
var
  i, j: Integer;
  Temp: Boolean;
begin
  for i := 0 to FCount div 2 - 1 do
  begin
    j := FCount - 1 - i;
    Temp := GetBit(i);
    SetBit(i, GetBit(j));
    SetBit(j, Temp);
  end {for};
end {procedure};

procedure TBooleanList.SetAll(Value: Boolean);
var
  i: Integer;
  Bits: TBits;
begin
  if Value then Bits := [0..(SizeOf(Integer) * 8 - 1)] else Bits := [];
  for i := 0 to ((FCount + BitsCount - 1) div BitsCount) - 1 do
    FList^[i] := Bits;
end {procedure};

{ TByteList } 

function TByteList.GetItems(Index: Integer): Byte;
begin
  CheckBounds(Index);
  Result := FList^[Index];
end {function};

procedure TByteList.SetItems(Index: Integer; const Value: Byte);
begin
  CheckBounds(Index);
  FList^[Index] := Value;
end {procedure};

function TByteList.FirstGrowCapacity: Integer;
begin
  Result := 4;
end {function};

function TByteList.MaxCapacity: Integer;
begin
  Result := MaxByteListSize;
end {function};

procedure TByteList.Reallocate(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(Byte));
end {procedure};

function TByteList.Add(Item: Byte): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end {function};

function TByteList.First: Byte;
begin
  CheckEmpty;
  Result := FList^[0];
end {function};

function TByteList.IdenticalTo(OtherList: TArrayBasedList): Boolean;
var
  i: Integer;
  OtherArray: PByteArray;
begin
  Result := (inherited IdenticalTo(OtherList)) and (OtherList is TByteList);
  if Result then
  begin
    OtherArray := TByteList(OtherList).List;
    for i := 0 to FCount - 1 do
      if FList^[i] <> OtherArray^[i] then
      begin
        Result := False;
        Break;
      end {if};
  end {if};
end {function};

function TByteList.IndexOf(Item: Byte): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FList^[i] = Item then
    begin
      Result := i;
      Break;
    end {if};
end {function};

function TByteList.Last: Byte;
begin
  CheckEmpty;
  Result := FList^[FCount - 1];
end {function};

function TByteList.Remove(Item: Byte): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end {function};

procedure TByteList.Append(OtherList: TArrayBasedList); 
var
  i, OldCount: Integer;
  OtherArray: PByteArray;
begin
  if not (OtherList is TByteList) then AssignError;
  OldCount := Count;
  inherited;
  OtherArray := TByteList(OtherList).List;
  for i := 0 to OtherList.Count - 1 do
    FList^[OldCount + i] := OtherArray^[i];
end {procedure};

procedure TByteList.Assign(OtherList: TArrayBasedList);
var
  i: Integer;
  OtherArray: PByteArray;
begin
  if not (OtherList is TByteList) then AssignError;
  inherited;
  OtherArray := TByteList(OtherList).List;
  for i := 0 to FCount - 1 do
    FList^[i] := OtherArray^[i];
end {procedure};

procedure TByteList.Delete(Index: Integer);
begin
  CheckBounds(Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(
      Byte));
end {procedure};

procedure TByteList.Exchange(Index1, Index2: Integer);
var
  Item: Byte;
begin
  inherited;
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end {procedure};

procedure TByteList.Insert(Index: Integer; Item: Byte);
begin
  if Index = FCount then
    Add(Item)
  else
  begin
    CheckBounds(Index);
    if FCount = FCapacity then
      Grow;
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(
      Byte));
    FList^[Index] := Item;
    Inc(FCount);
  end {else};  
end {procedure};

procedure TByteList.Move(CurIndex, NewIndex: Integer);
var
  Diff: Integer;
  Item: Byte;
begin
  inherited;
  Item := FList^[CurIndex];
  Diff := NewIndex - CurIndex;
  if Diff > 0 then
    System.Move(FList^[CurIndex + 1], FList^[CurIndex], Diff * SizeOf(Byte))
  else if Diff < 0 then
    System.Move(FList^[NewIndex], FList^[NewIndex + 1], -Diff * SizeOf(Byte));
  FList^[NewIndex] := Item;
end {procedure};

procedure TByteList.ReverseOrder;
var
  i, j: Integer;
  Temp: Byte;
begin
  for i := 0 to FCount div 2 - 1 do
  begin
    j := FCount - 1 - i;
    Temp := FList^[i];
    FList^[i] := FList^[j];
    FList^[j] := Temp;
  end {for};
end {procedure};

procedure TByteList.SetAll(Value: Byte);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    FList^[i] := Value;
end {procedure};

{ TEnhancedList }

function TEnhancedList.GetItems(Index: Integer): Pointer;
begin
  CheckBounds(Index);
  Result := FList^[Index];
end {function};

procedure TEnhancedList.SetItems(Index: Integer; const Value: Pointer);
begin
  CheckBounds(Index);
  FList^[Index] := Value;
end {procedure};

function TEnhancedList.FirstGrowCapacity: Integer;
begin
  Result := 4;
end {function};

function TEnhancedList.MaxCapacity: Integer;
begin
  Result := MaxListSize;
end {function};

procedure TEnhancedList.Reallocate(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(Pointer));
end {procedure};

function TEnhancedList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end {function};

function TEnhancedList.AddSorted(Item: Pointer; Compare: TListSortCompare):
  Integer;
begin
  BinarySearchPointer(Item, FList, FCount, Result, Compare);
  if Result = FCount then
    Add(Item)
  else
    Insert(Result, Item);
end {function};

function TEnhancedList.First: Pointer;
begin
  CheckEmpty;
  Result := FList^[0];
end {function};

function TEnhancedList.IdenticalTo(OtherList: TArrayBasedList): Boolean;
var
  i: Integer;
  OtherArray: PPointerList;
begin
  Result := (inherited IdenticalTo(OtherList)) and (OtherList is TEnhancedList);
  if Result then
  begin
    OtherArray := TEnhancedList(OtherList).List;
    for i := 0 to FCount - 1 do
      if FList^[i] <> OtherArray^[i] then
      begin
        Result := False;
        Break;
      end {if};
  end {if};
end {function};

function TEnhancedList.IndexOf(Item: Pointer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FList^[i] = Item then
    begin
      Result := i;
      Break;
    end {if};
end {function};

function TEnhancedList.IndexOfSorted(Item: Pointer; Compare: TListSortCompare):
  Integer;
begin
  if not BinarySearchPointer(Item, FList, FCount, Result, Compare) then
    Result := -1;
end {function};

function TEnhancedList.Iterate(StartIndex: Integer; IterateForward: Boolean;
 const IterateMethod: TIterateMethod): Integer;
var
  i: Integer;
  Continue: Boolean;
begin
  Result := - 1;
  Continue := True;
  CheckBounds(StartIndex);
  if not Assigned(IterateMethod) then
    Raise EListError.Create('IterateMethod not assigned');
  if IterateForward then
    for i := StartIndex to FCount - 1 do
    begin
      IterateMethod(FList^[i], Continue);
      if not Continue then
      begin
        Result := i;
        Break;
      end {if};
    end {for}
  else
    for i := StartIndex downto 0 do
    begin
      IterateMethod(FList^[i], Continue);
      if not Continue then
      begin
        Result := i;
        Break;
      end {if};
    end {for}
end {function};

function TEnhancedList.Last: Pointer;
begin
  CheckEmpty;
  Result := FList^[FCount - 1];
end {function};

function TEnhancedList.Remove(Item: Pointer; DeleteAction: TDeleteAction):
  Integer; 
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result, DeleteAction);
end {function};

procedure TEnhancedList.Append(OtherList: TArrayBasedList); 
var
  i, OldCount: Integer;
  OtherArray: PPointerList;
begin
  if not (OtherList is TEnhancedList) then AssignError;
  OldCount := Count;
  inherited;
  OtherArray := TEnhancedList(OtherList).List;
  for i := 0 to OtherList.Count - 1 do
    FList^[OldCount + i] := OtherArray^[i];
end {procedure};

procedure TEnhancedList.Assign(OtherList: TArrayBasedList);
var
  i: Integer;
  OtherArray: PPointerList;
begin
  if not (OtherList is TEnhancedList) then AssignError;
  inherited;
  OtherArray := TEnhancedList(OtherList).List;
  for i := 0 to FCount - 1 do
    FList^[i] := OtherArray^[i];
end {procedure};

procedure TEnhancedList.Clear(DeleteAction: TDeleteAction); 
var
  i: Integer;
begin
  if DeleteAction <> daDoNothing then
    for i := 0 to FCount - 1 do
      PerformDeleteAction(FList^[i], DeleteAction);
  inherited Clear;
end {procedure};

procedure TEnhancedList.Delete(Index: Integer; DeleteAction: TDeleteAction);
begin
  CheckBounds(Index);
  Dec(FCount);
  PerformDeleteAction(FList^[Index], DeleteAction); 
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(
      Pointer));
end {procedure};

procedure TEnhancedList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  inherited;
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end {procedure};

procedure TEnhancedList.FreeObjectsAndClear; 
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    TObject(FList^[i]).Free;
  inherited Clear;
end {procedure};

procedure TEnhancedList.Insert(Index: Integer; Item: Pointer);
begin
  if Index = FCount then
    Add(Item)
  else
  begin
    CheckBounds(Index);
    if FCount = FCapacity then
      Grow;
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(
      Pointer));
    FList^[Index] := Item;
    Inc(FCount);
  end {else};
end {procedure};

procedure TEnhancedList.Move(CurIndex, NewIndex: Integer);
var
  Diff: Integer;
  Item: Pointer;
begin
  inherited;
  Item := FList^[CurIndex];
  Diff := NewIndex - CurIndex;
  if Diff > 0 then
    System.Move(FList^[CurIndex + 1], FList^[CurIndex], Diff * SizeOf(Pointer))
  else if Diff < 0 then
    System.Move(FList^[NewIndex], FList^[NewIndex + 1], -Diff * SizeOf(Pointer));
  FList^[NewIndex] := Item;
end {procedure};

procedure TEnhancedList.Pack;
var
  i, NextIndexToPut: Integer;
begin
  NextIndexToPut := 0;
  for i := 0 to FCount - 1 do
    if FList^[i] = nil then
      Dec(FCount)
    else
    begin
      FList^[NextIndexToPut] := FList^[i];
      Inc(NextIndexToPut);
    end {else};
end {procedure};

procedure TEnhancedList.ReverseOrder; 
var
  i, j: Integer;
  Temp: Pointer;
begin
  for i := 0 to FCount div 2 - 1 do
  begin
    j := FCount - 1 - i;
    Temp := FList^[i];
    FList^[i] := FList^[j];
    FList^[j] := Temp;
  end {for};
end {procedure};

procedure TEnhancedList.SetAll(Value: Pointer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    FList^[i] := Value;
end {procedure};

procedure TEnhancedList.Sort(Compare: TListSortCompare);
begin
  if (FList <> nil) and (FCount > 0) then
    QuickSortPointer(FList, 0, FCount - 1, Compare);
end {procedure};

{ TFloatList }

function TFloatList.GetItems(Index: Integer): Double;
begin
  CheckBounds(Index);
  Result := FList^[Index];
end {function};

procedure TFloatList.SetItems(Index: Integer; const Value: Double);
begin
  CheckBounds(Index);
  FList^[Index] := Value;
end {procedure};

function TFloatList.FirstGrowCapacity: Integer;
begin
  Result := 4;
end {function};

function TFloatList.MaxCapacity: Integer;
begin
  Result := MaxFloatListSize;
end {function};

procedure TFloatList.Reallocate(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(Double));
end {procedure};

function TFloatList.Add(Item: Double): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end {function};

function TFloatList.AddSorted(Item: Double): Integer;
begin
  BinarySearchFloat(Item, FList, FCount, Result);
  if Result = FCount then
    Add(Item)
  else
    Insert(Result, Item);
end {function};

function TFloatList.First: Double;
begin
  CheckEmpty;
  Result := FList^[0];
end {function};

function TFloatList.IdenticalTo(OtherList: TArrayBasedList): Boolean;
var
  i: Integer;
  OtherArray: PFloatArray;
begin
  Result := (inherited IdenticalTo(OtherList)) and (OtherList is TFloatList);
  if Result then
  begin
    OtherArray := TFloatList(OtherList).List;
    for i := 0 to FCount - 1 do
      if FList^[i] <> OtherArray^[i] then
      begin
        Result := False;
        Break;
      end {if};
  end {if};
end {function};

function TFloatList.IndexOf(Item: Double): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FList^[i] = Item then
    begin
      Result := i;
      Break;
    end {if};
end {function};

function TFloatList.IndexOfSorted(Item: Double): Integer;
begin
  if not BinarySearchFloat(Item, FList, FCount, Result) then
    Result := -1;
end {function};

function TFloatList.Last: Double;
begin
  CheckEmpty;
  Result := FList^[FCount - 1];
end {function};

function TFloatList.Mean: Extended;
begin
  CheckEmpty;
  Result := Math.Mean(Slice(FList^, FCount));
end {function};

function TFloatList.Remove(Item: Double): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end {function};

function TFloatList.StdDev: Extended;
begin
  CheckEmpty;
  if FCount = 1 then
    Result := 0
  else
    Result := Math.StdDev(Slice(FList^, FCount));
end {function};

function TFloatList.Sum: Extended;
begin
  if FCount = 0 then Result := 0 else Result := Math.Sum(Slice(FList^, FCount));
end {function};

procedure TFloatList.Append(OtherList: TArrayBasedList); 
var
  i, OldCount: Integer;
  OtherArray: PFloatArray;
begin
  if not (OtherList is TFloatList) then AssignError;
  OldCount := Count;
  inherited;
  OtherArray := TFloatList(OtherList).List;
  for i := 0 to OtherList.Count - 1 do
    FList^[OldCount + i] := OtherArray^[i];
end {procedure};

procedure TFloatList.Assign(OtherList: TArrayBasedList);
var
  i: Integer;
  OtherArray: PFloatArray;
begin
  if not (OtherList is TFloatList) then AssignError;
  inherited;
  OtherArray := TFloatList(OtherList).List;
  for i := 0 to FCount - 1 do
    FList^[i] := OtherArray^[i];
end {procedure};

procedure TFloatList.Delete(Index: Integer);
begin
  CheckBounds(Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(
      Double));
end {procedure};

procedure TFloatList.Exchange(Index1, Index2: Integer);
var
  Item: Double;
begin
  inherited;
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end {procedure};

procedure TFloatList.Insert(Index: Integer; Item: Double);
begin
  if Index = FCount then
    Add(Item)
  else
  begin
    CheckBounds(Index);
    if FCount = FCapacity then
      Grow;
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(
      Double));
    FList^[Index] := Item;
    Inc(FCount);
  end {else};  
end {procedure};

procedure TFloatList.MeanAndStdDev(var Mean, StdDev: Extended);
begin
  CheckEmpty;
  if FCount = 1 then
  begin
    Mean := FList^[0];
    StdDev := 0;
  end {if}
  else
    Math.MeanAndStdDev(Slice(FList^, FCount), Mean, StdDev);
end {procedure};

procedure TFloatList.Move(CurIndex, NewIndex: Integer);
var
  Diff: Integer;
  Item: Double;
begin
  inherited;
  Item := FList^[CurIndex];
  Diff := NewIndex - CurIndex;
  if Diff > 0 then
    System.Move(FList^[CurIndex + 1], FList^[CurIndex], Diff * SizeOf(Double))
  else if Diff < 0 then
    System.Move(FList^[NewIndex], FList^[NewIndex + 1], -Diff * SizeOf(Double));
  FList^[NewIndex] := Item;
end {procedure};

procedure TFloatList.ReverseOrder; 
var
  i, j: Integer;
  Temp: Double;
begin
  for i := 0 to FCount div 2 - 1 do
  begin
    j := FCount - 1 - i;
    Temp := FList^[i];
    FList^[i] := FList^[j];
    FList^[j] := Temp;
  end {for};
end {procedure};

procedure TFloatList.SetAll(Value: Double);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    FList^[i] := Value;
end {procedure};

procedure TFloatList.Sort;
begin
  if (FList <> nil) and (FCount > 0) then
    QuickSortFloat(FList, 0, FCount - 1);
end {procedure};

{ TIntegerList }

function TIntegerList.GetItems(Index: Integer): Integer;
begin
  CheckBounds(Index);
  Result := FList^[Index];
end {function};

procedure TIntegerList.SetItems(Index: Integer; const Value: Integer);
begin
  CheckBounds(Index);
  FList^[Index] := Value;
end {procedure};

function TIntegerList.FirstGrowCapacity: Integer;
begin
  Result := 4;
end {function};

function TIntegerList.MaxCapacity: Integer;
begin
  Result := MaxIntegerListSize;
end {function};

procedure TIntegerList.Reallocate(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(Integer));
end {procedure};

function TIntegerList.Add(Item: Integer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end {function};

function TIntegerList.AddSorted(Item: Integer): Integer;
begin
  BinarySearchInteger(Item, FList, FCount, Result);
  if Result = FCount then
    Add(Item)
  else
    Insert(Result, Item);
end {function};

function TIntegerList.First: Integer;
begin
  CheckEmpty;
  Result := FList^[0];
end {function};

function TIntegerList.IdenticalTo(OtherList: TArrayBasedList): Boolean;
var
  i: Integer;
  OtherArray: PIntegerArray;
begin
  Result := (inherited IdenticalTo(OtherList)) and (OtherList is TIntegerList);
  if Result then
  begin
    OtherArray := TIntegerList(OtherList).List;
    for i := 0 to FCount - 1 do
      if FList^[i] <> OtherArray^[i] then
      begin
        Result := False;
        Break;
      end {if};
  end {if};
end {function};

function TIntegerList.IndexOf(Item: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FList^[i] = Item then
    begin
      Result := i;
      Break;
    end {if};
end {function};

function TIntegerList.IndexOfSorted(Item: Integer): Integer;
begin
  if not BinarySearchInteger(Item, FList, FCount, Result) then
    Result := -1;
end {function};

function TIntegerList.Last: Integer;
begin
  CheckEmpty;
  Result := FList^[FCount - 1];
end {function};

function TIntegerList.Remove(Item: Integer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end {function};

function TIntegerList.Sum: Integer;
begin
  if FCount = 0 then Result := 0 else Result := Math.SumInt(Slice(FList^,
    FCount));
end {function};

procedure TIntegerList.Append(OtherList: TArrayBasedList); 
var
  i, OldCount: Integer;
  OtherArray: PIntegerArray;
begin
  if not (OtherList is TIntegerList) then AssignError;
  OldCount := Count;
  inherited;
  OtherArray := TIntegerList(OtherList).List;
  for i := 0 to OtherList.Count - 1 do
    FList^[OldCount + i] := OtherArray^[i];
end {procedure};

procedure TIntegerList.Assign(OtherList: TArrayBasedList);
var
  i: Integer;
  OtherArray: PIntegerArray;
begin
  if not (OtherList is TIntegerList) then AssignError;
  inherited;
  OtherArray := TIntegerList(OtherList).List;
  for i := 0 to FCount - 1 do
    FList^[i] := OtherArray^[i];
end {procedure};

procedure TIntegerList.Delete(Index: Integer);
begin
  CheckBounds(Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(
      Integer));
end {procedure};

procedure TIntegerList.Exchange(Index1, Index2: Integer);
var
  Item: Integer;
begin
  inherited;
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end {procedure};

procedure TIntegerList.Insert(Index: Integer; Item: Integer);
begin
  if Index = FCount then
    Add(Item)
  else
  begin
    CheckBounds(Index);
    if FCount = FCapacity then
      Grow;
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(
      Integer));
    FList^[Index] := Item;
    Inc(FCount);
  end {else};  
end {procedure};

procedure TIntegerList.Move(CurIndex, NewIndex: Integer);
var
  Diff: Integer;
  Item: Integer;
begin
  inherited;
  Item := FList^[CurIndex];
  Diff := NewIndex - CurIndex;
  if Diff > 0 then
    System.Move(FList^[CurIndex + 1], FList^[CurIndex], Diff * SizeOf(Integer))
  else if Diff < 0 then
    System.Move(FList^[NewIndex], FList^[NewIndex + 1], -Diff * SizeOf(Integer));
  FList^[NewIndex] := Item;
end {procedure};

procedure TIntegerList.ReverseOrder; 
var
  i, j, Temp: Integer;
begin
  for i := 0 to FCount div 2 - 1 do
  begin
    j := FCount - 1 - i;
    Temp := FList^[i];
    FList^[i] := FList^[j];
    FList^[j] := Temp;
  end {for};
end {procedure};

procedure TIntegerList.SetAll(Value: Integer);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FList^[i] := Value;
end {procedure};

procedure TIntegerList.Sort;
begin
  if (FList <> nil) and (FCount > 0) then
    QuickSortInteger(FList, 0, FCount - 1);
end {procedure};

{ TDoubleLinkList }

function TDoubleLinkList.GetCurrentItem: Pointer;
begin
{  if Current <> nil then Result := Current^.Item else Result := nil;}
  Result := Current^.Item; {NilNode enables us to do this}
end {function};

function TDoubleLinkList.GetEndOfList: Boolean;
begin
  Result := Current = NilNode;
end {function};

function TDoubleLinkList.GetItems(Index: Integer): Pointer;
begin
  SetCurrentIndex(Index);
  Result := Current^.Item;
end {function};

procedure TDoubleLinkList.SetCurrentIndex(const Value: Integer);

  procedure ScanBackward;
  begin
    while FCurrentIndex <> Value do
    begin
      Current := Current^.Previous;
      Dec(FCurrentIndex);
    end {while};
  end {procedure};

  procedure ScanForward;
  begin
    while FCurrentIndex <> Value do
    begin
      Current := Current^.Next;
      Inc(FCurrentIndex);
    end {while};
  end {procedure};

var
  DistanceFromCurrent: Integer;
begin
  if (Value < 0) or (Value >= FCount) then
    raise EListError.Create('List index out of bounds');
  if Current <> NilNode then
  begin
    DistanceFromCurrent := Value - FCurrentIndex;
    if DistanceFromCurrent > 0 then
    begin
      if FCount - Value - 1 < DistanceFromCurrent then {tail closer than current}
      begin
        Last;
        ScanBackward;
      end {if}
      else ScanForward;
    end {if}
    else if Value < -DistanceFromCurrent then {head closer than current}
    begin
      First;
      ScanForward;
    end {else if}
    else ScanBackward;
  end {if}
  else if Value < FCount - Value then {head is closer than tail}
  begin
    First;
    ScanForward;
  end {else if}
  else {tail is closer than head}
  begin
    Last;
    ScanBackward;
  end {else};
end {procedure};

procedure TDoubleLinkList.SetCurrentItem(const Value: Pointer);
begin
  RaiseExceptionIfNoCurrent;
  Current^.Item := Value;
end {procedure};

procedure TDoubleLinkList.SetItems(Index: Integer; const Value: Pointer);
begin
  SetCurrentIndex(Index);
  Current^.Item := Value;
end {procedure};

procedure TDoubleLinkList.RaiseExceptionIfNoCurrent;
begin
  if Current = NilNode then
    raise EListError.Create('No current item');
end {procedure};

function TDoubleLinkList.Find(Item: Pointer): Boolean;
begin
  Result := False;
  First;
  while Current <> NilNode do
    if Item = Current^.Item then
    begin
      Result := True;
      Break;
    end {if}
    else
    begin
      Current := Current^.Next;
      Inc(FCurrentIndex);
    end {else};
  if not Result then FCurrentIndex := -1;
end {function};

procedure TDoubleLinkList.Clear(DeleteAction: TDeleteAction);
var
  RecordToFree: PListRecord;
begin
  Current := Head;
  while Current <> NilNode do
  begin
    PerformDeleteAction(Current^.Item, DeleteAction); 
    RecordToFree := Current;
    Current := Current^.Next;
    Dispose(RecordToFree);
  end {while};
  Head := NilNode;
  Tail := NilNode;
  FCount := 0;
  FCurrentIndex := -1;
end {procedure};

procedure TDoubleLinkList.Delete(DeleteAction: TDeleteAction);
var
  NewCurrent: PListRecord;
begin
  RaiseExceptionIfNoCurrent;
  PerformDeleteAction(Current^.Item, DeleteAction); 
  if FCurrentIndex = 0 then {delete head, >= 1 item before delete}
  begin
    NewCurrent := Current^.Next;
    Head := NewCurrent;
    if Head <> NilNode then
      Head^.Previous := NilNode
    else {only 1 item in list before deletion}
    begin
      Tail := NilNode;
      FCurrentIndex := -1;
    end {else};
  end {if}
  else if FCurrentIndex = FCount - 1 then {delete tail, >= 2 items before delete}
  begin
    NewCurrent := Current^.Previous;
    Tail := NewCurrent;
    Tail^.Next := NilNode;
    Dec(FCurrentIndex);
  end {else if}
  else {internal deletion, >= 3 items before delete}
  begin
    NewCurrent := Current^.Next;
    Current^.Previous^.Next := NewCurrent;
    NewCurrent^.Previous := Current^.Previous;
  end {else};
  Dec(FCount);
  Dispose(Current);
  Current := NewCurrent;
end {procedure};

procedure TDoubleLinkList.First;
begin
  Current := Head;
  if Current <> NilNode then FCurrentIndex := 0; {otherwise it is already -1}
end {procedure};

procedure TDoubleLinkList.Insert(Item: Pointer; Position: TInsertPosition);
var
  ListRecord: PListRecord;
begin
  if Position in [ipBeforeCurrent, ipAfterCurrent] then
    RaiseExceptionIfNoCurrent;
  New(ListRecord);
  ListRecord^.Item := Item;
  case Position of
    ipAtBeginning:
    begin
      ListRecord^.Previous := NilNode;
      ListRecord^.Next := Head;
      if FCount = 0 then
        Tail := ListRecord
      else Head^.Previous := ListRecord;
      Head := ListRecord;
      FCurrentIndex := 0;
    end {ipAtBeginning};
    ipAtEnd:
    begin
      ListRecord^.Next := NilNode;
      ListRecord^.Previous := Tail;
      if FCount = 0 then
        Head := ListRecord
      else Tail^.Next := ListRecord;
      Tail := ListRecord;
      FCurrentIndex := FCount;
    end {ipAtEnd};
    ipBeforeCurrent:
    begin
      ListRecord^.Next := Current;
      ListRecord^.Previous := Current^.Previous;
      if ListRecord^.Previous = NilNode then
        Head := ListRecord
      else
        ListRecord^.Previous^.Next := ListRecord;
      Current^.Previous := ListRecord;
    end {ipBeforeCurrent};
    ipAfterCurrent:
    begin
      ListRecord^.Previous := Current;
      ListRecord^.Next := Current^.Next;
      if ListRecord^.Next = NilNode then
        Tail := ListRecord
      else
        ListRecord^.Next^.Previous := ListRecord;
      Current^.Next := ListRecord;
      Inc(FCurrentIndex);
    end {ipAfterCurrent};
  end {case};
  Current := ListRecord;
  Inc(FCount);
end {procedure};

procedure TDoubleLinkList.Iterate(Alternative: TIterateAlternative; const
  IterateMethod: TIterateMethod);
var
  Continue: Boolean;

  procedure IterateBackward;
  begin
    while Current <> NilNode do
    begin
      IterateMethod(Current^.Item, Continue);
      if Continue then
      begin
        Current := Current^.Previous;
        Dec(FCurrentIndex);
      end {if}
      else Break;
    end {while};
  end {procedure};

  procedure IterateForward;
  begin
    while Current <> NilNode do
    begin
      IterateMethod(Current^.Item, Continue);
      if Continue then
      begin
        Current := Current^.Next;
        Inc(FCurrentIndex);
      end {if}
      else Break;
    end {while};
    if Current = NilNode then FCurrentIndex := -1;
  end {procedure};

begin
  if not Assigned(IterateMethod) then
    raise EListError.Create('Iterate method not assigned');
  Continue := True;
  case Alternative of
    iaForwardFromBeginning:
    begin
      First;
      IterateForward;
    end {iaForwardFromBeginning};
    iaForwardFromCurrent:
    begin
      RaiseExceptionIfNoCurrent;
      IterateForward;
    end {iaForwardFromCurrent};
    iaBackwardFromCurrent:
    begin
      RaiseExceptionIfNoCurrent;
      IterateBackward;
    end {iaBackwardFromCurrent};
    iaBackwardFromEnd:
    begin
      Last;
      IterateBackward;
    end {iaBackwardFromCurrent};
  end {case};
end {procedure};

procedure TDoubleLinkList.Last;
begin
  Current := Tail;
  FCurrentIndex := FCount - 1; {Works also if empty}
end {procedure};

procedure TDoubleLinkList.Next;
begin
  RaiseExceptionIfNoCurrent;
  Current := Current^.Next;
  if Current <> NilNode then
    Inc(FCurrentIndex)
  else FCurrentIndex := -1;
end {procedure};

procedure TDoubleLinkList.Prior;
begin
  RaiseExceptionIfNoCurrent;
  Current := Current^.Previous;
  Dec(FCurrentIndex); {automatically goes to -1 if Current becomes NilNode}
end {procedure};

constructor TDoubleLinkList.Create;
begin
  inherited Create;
  FCurrentIndex := -1;
  New(NilNode);
  NilNode^.Next := NilNode;
  NilNode^.Previous := NilNode;
  NilNode^.Item := nil;
  Current := NilNode;
  Head := NilNode;
  Tail := NilNode;
end {constructor};

destructor TDoubleLinkList.Destroy;
begin
  Clear;
  Dispose(NilNode);
  inherited;
end {destructor};

end.


