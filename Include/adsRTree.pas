unit adsRTree;

interface

uses
  Classes, adsGeometry;

type
  TadsRTree = class;
  TadsRTreeNode = class;
  {$ifdef DEBUGLOG}
  TadsRTreeLogEvent = procedure(Sender: TObject; const Msg: String) of object;
  {$endif}

  TadsRTreeEntry = class
  protected
    ID: Integer;
    Rect: TadsRect;
    Child: TadsRTreeNode;
    Container: TadsRTreeNode;
  protected
    {$ifdef DEBUGLOG}
    procedure Dump(Strings: TStrings);
    {$endif}
  public
    constructor Create(aChild: TadsRTreeNode); overload;
    constructor Create(aRect: TadsRect; aID: Integer); overload;
  end{class};

  TadsRTreeNode = class
  protected
    Leaf: Boolean;
    Rect: TadsRect;
    Tree: TadsRTree;
    Parent: TadsRTreeEntry;
    NoOfEntries: Integer;
    Entries: Array of TadsRTreeEntry;
    procedure RecalculateRect;
    function AddEntry(Entry: TadsRTreeEntry): Boolean; virtual;
  protected
    {$ifdef DEBUGLOG}
    InternalID: Integer;
    procedure Dump(Strings: TStrings);
    {$endif}
  public
    constructor Create(MaxEntries: Integer; IsLeaf: Boolean; Owner: TadsRTree);
  end{class};

  TadsRTree = class
  private
    {$ifdef DEBUGLOG}
    FOnLog: TadsRTreeLogEvent;
    {$endif}
  protected
    Root: TadsRTreeNode;
    MinNoOfEntries: Integer;
    MaxNoOfEntries: Integer;

    {$ifdef DEBUGLOG}
    HighestInternalID: Integer;
    procedure DoLog(const Msg: String); overload;
    procedure DoLog(const Msg: String; Args: Array of const); overload;
    {$endif}

    function CreateNode(MaxEntries: Integer; IsLeaf: Boolean): TadsRTreeNode; virtual;
    function CreateEntry(aChild: TadsRTreeNode): TadsRTreeEntry; overload; virtual;
    function CreateEntry(aRect: TadsRect; aID: Integer): TadsRTreeEntry; overload; virtual;

    {** Returns the leaf node that needs to be expanded least in order to
     accomodate the new node. At this point, we don't know if the new node will
     actually fit in the selected node, or if the selected node needs to be
     split in two. }
    function SelectNode(Entry: TadsRTreeEntry): TadsRTreeNode; virtual;

    {** This method is called if FullNode can't hold any more entries. It
     distributes all old entries in FullNode together with NewEntry among
     FullNode and EmptyNode. It uses the "quadratic split" algorithm described
     by Guttman. }
    procedure SplitNode(FullNode, EmptyNode: TadsRTreeNode; NewEntry: TadsRTreeEntry);

    {** Ascends from a leaf node to the root, adjusting covering rectangles and
     propagating node splits as necessary. }
    procedure AdjustTree(var Node, NewNode: TadsRTreeNode);

  public

    {** Insert a new entry, with given rectangle and ID, into the tree. }
    procedure Insert(Rect: TadsRect; ID: Integer); virtual;

    {** todo comment}
    procedure Clear;

    {$ifdef DEBUGLOG}
    procedure Dump(Strings: TStrings);
    {$endif}

    {$ifdef DEBUGLOG}
    constructor Create(MinEntries, MaxEntries: Integer; LogHandler: TadsRTreeLogEvent);
    {$else}
    constructor Create(MinEntries, MaxEntries: Integer);
    {$endif}
    destructor Destroy; override;
  end{class};

implementation

uses
  Contnrs
  {$ifdef DEBUGLOG}
  , SysUtils
  {$endif}
  ;

{$ifdef DEBUGLOG}
var
  Indent: Integer;
{$endif}

{---------- TadsRTreeEntry ----------------------------------------------------}

{$ifdef DEBUGLOG}
procedure TadsRTreeEntry.Dump(Strings: TStrings);
begin
  if Container.Leaf then
    Strings.Add(StringOfChar(' ', Indent) + Format('ENTRY, RECT=(%f,%f)-(%f,%f), ID=%d',
      [Rect.TopLeft.X, Rect.TopLeft.Y, Rect.BottomRight.X, Rect.BottomRight.Y, ID]))
  else
  begin
    Strings.Add(StringOfChar(' ', Indent) + Format('ENTRY, RECT=(%f,%f)-(%f,%f)',
      [Rect.TopLeft.X, Rect.TopLeft.Y, Rect.BottomRight.X, Rect.BottomRight.Y]));
    Child.Dump(Strings);
  end{else};
end{procedure};
{$endif}

constructor TadsRTreeEntry.Create(aRect: TadsRect; aID: Integer);
begin
  ID := aID;
  Rect := aRect;
  Child := nil;
end{constructor};

constructor TadsRTreeEntry.Create(aChild: TadsRTreeNode);
begin
  ID := -1;
  Rect := aChild.Rect;
  Child := aChild;
  Child.Parent := Self;
end{constructor};

{---------- TadsRTreeNode -----------------------------------------------------}

procedure TadsRTreeNode.RecalculateRect;
var
  i: Integer;
begin
  if NoOfEntries = 0 then
    Rect := adsRect(0, 0, 0, 0)
  else
  begin
    Rect := Entries[0].Rect;
    for i := 1 to NoOfEntries-1 do
      Rect := Union(Rect, Entries[i].Rect);
  end{else};
end{procedure};

function TadsRTreeNode.AddEntry(Entry: TadsRTreeEntry): Boolean;
begin
  if NoOfEntries < Length(Entries) then
  begin
    Entries[NoOfEntries] := Entry;
    Inc(NoOfEntries);
    if NoOfEntries = 1 then
      Rect := Entry.Rect
    else
      Rect := Union(Rect, Entry.Rect);
    Entry.Container := Self;
    Result := True;
  end{if}
  else
    Result := False;
  {$ifdef DEBUGLOG}
  if Result then
  begin
    Tree.DoLog('Adding entry with rectangle (%f,%f)-(%f,%f) to node with internal ID %d',
      [Entry.Rect.TopLeft.X, Entry.Rect.TopLeft.Y, Entry.Rect.BottomRight.X,
       Entry.Rect.BottomRight.Y, InternalID]);
    Tree.DoLog('Rectangle for node with internal id %d is now (%f,%f)-(%f,%f)', [InternalID,
      Rect.TopLeft.X, Rect.TopLeft.Y, Rect.BottomRight.X, Rect.BottomRight.Y]);
  end{if};
  {$endif}
end{function};

{$ifdef DEBUGLOG}
procedure TadsRTreeNode.Dump(Strings: TStrings);
var
  i: Integer;
  S: String;
begin
  if Leaf then
    S := StringOfChar(' ', Indent) + 'LEAF NODE %d , RECT=(%f,%f)-(%f,%f)'
  else
    S := StringOfChar(' ', Indent) + 'NODE %d, RECT=(%f,%f)-(%f,%f)';
  Strings.Add(Format(S, [InternalID, Rect.TopLeft.X, Rect.TopLeft.Y, Rect.BottomRight.X,
    Rect.BottomRight.Y]));
  Inc(Indent, 2);
  for i := 0 to NoOfEntries-1 do
    Entries[i].Dump(Strings);
  Dec(Indent, 2);
end{procedure};
{$endif}

constructor TadsRTreeNode.Create(MaxEntries: Integer; IsLeaf: Boolean; Owner: TadsRTree);
begin
  Tree := Owner;
  Leaf := IsLeaf;
  NoOfEntries := 0;
  SetLength(Entries, MaxEntries);
end{constructor};

{---------- TadsRTree ---------------------------------------------------------}

{$ifdef DEBUGLOG}
procedure TadsRTree.DoLog(const Msg: String);
begin
  DoLog(Msg, []);
end{procedure};

procedure TadsRTree.DoLog(const Msg: String; Args: Array of const);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, DateTimeToStr(Now) + ';' + Format(Msg, Args));
end{procedure};
{$endif}

function TadsRTree.CreateNode(MaxEntries: Integer; IsLeaf: Boolean): TadsRTreeNode;
begin
  {$ifdef DEBUGLOG}
  Inc(HighestInternalID);
  {$endif}
  Result := TadsRTreeNode.Create(MaxEntries, IsLeaf, Self);
  {$ifdef DEBUGLOG}
  Result.InternalID := HighestInternalID;
  DoLog('Created node with internal ID %d', [Result.InternalID]);
  {$endif}
end{function};

function TadsRTree.CreateEntry(aChild: TadsRTreeNode): TadsRTreeEntry;
begin
  Result := TadsRTreeEntry.Create(aChild);
end{function};

function TadsRTree.CreateEntry(aRect: TadsRect; aID: Integer): TadsRTreeEntry;
begin
  Result := TadsRTreeEntry.Create(aRect, aID);
end{function};

function TadsRTree.SelectNode(Entry: TadsRTreeEntry): TadsRTreeNode;
var
  i: Integer;
  RectUnion: TadsRect;
  Node: TadsRTreeNode;
  SelectedEntry: TadsRTreeEntry;
  Enlargement, LeastEnlargement: Double;
begin
  Node := Root;
  while not Node.Leaf do
  begin
    LeastEnlargement := MAXINT;
    SelectedEntry := Node.Entries[0];
    for i := 0 to Node.NoOfEntries-1 do
    begin
      RectUnion := Union(Node.Entries[i].Rect, Entry.Rect);
      Enlargement := Area(RectUnion) - Area(Node.Entries[i].Rect);
      if (Enlargement < LeastEnlargement) or ((Enlargement = LeastEnlargement) and
        (Area(RectUnion) < Area(Union(Entry.Rect, Node.Entries[i].Rect)))) and
        (Node.Leaf) then
      begin
        SelectedEntry := Node.Entries[i];
        LeastEnlargement := Enlargement;
      end{if};
    end{for};
    Node := SelectedEntry.Child;
  end{while};
  Result := Node;
end{function};

procedure TadsRTree.SplitNode(FullNode, EmptyNode: TadsRTreeNode;
  NewEntry: TadsRTreeEntry);
var
  i: Integer;
  AllEntries: TObjectList;

  function EntryAt(Index: Integer): TadsRTreeEntry;
  begin
    Result := AllEntries[Index] as TadsRTreeEntry;
  end{function};

  {
    For each pair of entries, compose a rectangle which is the union of them
    both. Calculate D = area (union) - area (E1) - area (E2). The pair with the
    largest D are our seeds and will be placed in the empty nodes.
  }
  procedure PickSeeds;
  var
    i, j: Integer;
    R1, R2: TadsRect;
    D, LargestD: Double;
    E1, E2: TadsRTreeEntry;
  begin
    E1 := EntryAt(0);
    E2 := EntryAt(1);
    LargestD := 0;
    for i := 0 to AllEntries.Count - 1 do
    begin
      for j := i + 1 to AllEntries.Count - 1 do
      begin
        R1 := EntryAt(i).Rect;
        R2 := EntryAt(j).Rect;
        D := Area(Union(R1, R2)) - Area(R1) - Area(R2);
        if D > LargestD then
        begin
          E1 := EntryAt(i);
          E2 := EntryAt(j);
          LargestD := D;
        end{if};
      end{for};
    end{for};
    FullNode.AddEntry(E1);
    AllEntries.Remove(E1);
    EmptyNode.AddEntry(E2);
    AllEntries.Remove(E2);
  end{procedure};

  {
    Look at the entries in AllEntries. For each entry, calculate D1 = the area
    increase required in the covering rectangle of FullNode to include the entry.
    Calculate D2 similarly for EmptyNode. Choose the entry with the maximum
    difference between D1 and D2, and place it into the node whose bounding
    rectangle needs the least enlargement. Resolve ties by adding the entry to
    the node with smaller area, then to the one with fewer entries, then to
    either.
  }
  procedure PickNextEntry;
  var
    i: Integer;
    D1, D2, MaxDiff: Double;
    SelectedNode: TadsRTreeNode;
    SelectedEntry: TadsRTreeEntry;
  begin
    MaxDiff := 0;
    SelectedNode := FullNode;
    SelectedEntry := EntryAt(0);
    for i := 0 to AllEntries.Count-1 do
    begin
      D1 := Area(Union(EntryAt(i).Rect, FullNode.Rect)) - Area(FullNode.Rect);
      D2 := Area(Union(EntryAt(i).Rect, EmptyNode.Rect)) - Area(EmptyNode.Rect);
      if Abs(D1-D2) > MaxDiff then
      begin
        SelectedEntry := EntryAt(i);
        MaxDiff := Abs(D1-D2);
        if D1 < D2 then
          SelectedNode := FullNode
        else
          SelectedNode := EmptyNode;
      end{if}
      else if Abs(D1-D2) = MaxDiff then
      begin
        SelectedEntry := EntryAt(i);
        if Area(FullNode.Rect) < Area(EmptyNode.Rect) then
          SelectedNode := FullNode
        else if Area(EmptyNode.Rect) < Area(FullNode.Rect) then
          SelectedNode := EmptyNode
        else if FullNode.NoOfEntries < EmptyNode.NoOfEntries then
          SelectedNode := FullNode
        else
          SelectedNode := EmptyNode;
      end{if};
    end{for};
    SelectedNode.AddEntry(SelectedEntry);
    AllEntries.Remove(SelectedEntry);
  end{procedure};

begin
  AllEntries := TObjectList.Create(False);
  try
    {
      Store the old entries, together with the new one, in AllEntries, and then
      clear FullNode. We now have two empty nodes and a list of entries. The job
      is to distribute the entries among the two nodes.
    }
    for i := 0 to FullNode.NoOfEntries - 1 do
      AllEntries.Add(FullNode.Entries[i]);
    AllEntries.Add(NewEntry);
    FullNode.NoOfEntries := 0;
    {
      Pick two seeds, i.e. the two entries that will go into the empty nodes.
    }
    PickSeeds;
    {
      For all remaining entries: If one node has so few entries that all the
      rest must be assigned to it in order for it to have the minimum number
      of entries, assign them all. If not, invoke PickNext to assign the next
      entry to one of the two nodes.
    }
    while AllEntries.Count > 0 do
    begin
      if FullNode.NoOfEntries + AllEntries.Count <= MinNoOfEntries then
        while AllEntries.Count > 0 do
        begin
          FullNode.AddEntry(EntryAt(0));
          AllEntries.Delete(0);
        end{while}
      else if EmptyNode.NoOfEntries + AllEntries.Count <= MinNoOfEntries then
        while AllEntries.Count > 0 do
        begin
          EmptyNode.AddEntry(EntryAt(0));
          AllEntries.Delete(0);
        end{while}
      else
        PickNextEntry;
    end{while};

  finally
    AllEntries.Free;
  end{finally};
end{procedure};

procedure TadsRTree.AdjustTree(var Node, NewNode: TadsRTreeNode);
var
  NewEntry: TadsRTreeEntry;
begin
  {
    If Node equals Root, it means that we split the root node in two. No
    adjustment is needed here; a new root will be created to hold the two nodes.
  }
  while Assigned(Node.Parent) do
  begin
    {
      Adjust the parent entry's covering rectangle so that it equals the node's,
      i.e. so that it tightly encloses all entry rectangles in the node.
    }
    Node.Parent.Rect := Node.Rect;
    Node.Parent.Container.RecalculateRect;

    {
      Propagate a node split upward: if a new node has been created due to a
      split, create a new entry containing the new node and try to add it to the
      parent node. If successful, we don't need to check for a split on the
      parent node, but only adjust bounding rectangles. If we couldn't add the
      new entry to the parent node, we create a new node and split the parent
      node, so that the two resulting nodes contain all old entries as well as
      the new entry. In this case, the next time this code is executed, it will
      handle the split parent node.
    }
    if Assigned(NewNode) then
    begin
      NewEntry := CreateEntry(NewNode);
      if Node.Parent.Container.AddEntry(NewEntry) then
        NewNode := nil
      else
      begin
        NewNode := CreateNode(MaxNoOfEntries, False);
        SplitNode(Node.Parent.Container, NewNode, NewEntry);
      end{else};
    end{if};
    Node := Node.Parent.Container;
  end{if};
end{procedure};

procedure TadsRTree.Insert(Rect: TadsRect; ID: Integer);
var
  Entry: TadsRTreeEntry;
  Node, NewNode: TadsRTreeNode;
begin

  {
    We start by creating a new entry, and determining into which node it should
    go, if possible.
  }
  Entry := CreateEntry(Rect, ID);
  Node := SelectNode(Entry);
  {
    If the selected node has room for another entry, it is inserted. Otherwise
    SplitNode will split the node up into two nodes. These two nodes will
    contain all the old entries from Node, plus the new entry.
  }
  if Node.AddEntry(Entry) then
    NewNode := nil
  else
  begin
    NewNode := CreateNode(MaxNoOfEntries, True);
    NewNode.Parent := Node.Parent;
    SplitNode(Node, NewNode, Entry);
  end{if};

  {
    Propagate changes upward. Invoke AdjustTree on Node, also passing NewNode if
    a split was performed
  }
  AdjustTree(Node, NewNode);

  {
    Grow tree taller. If node split propagation caused the root to split, create
    a new root whose children are the two resulting nodes.
  }
  if (not Assigned(Node.Parent)) and Assigned(NewNode) then
  begin
    Root := CreateNode(MaxNoOfEntries, False);
    Entry := CreateEntry(Node);
    Root.AddEntry(Entry);
    Entry := CreateEntry(NewNode);
    Root.AddEntry(Entry);
  end{if};
end{procedure};

procedure TadsRTree.Clear;
begin
  // todo clear everything, leaving an empty root
end{procedure};

{$ifdef DEBUGLOG}
procedure TadsRTree.Dump(Strings: TStrings);
begin
  Strings.Clear;
  Indent := 0;
  Root.Dump(Strings);
end{procedure};
{$endif}

{$ifdef DEBUGLOG}
constructor TadsRTree.Create(MinEntries, MaxEntries: Integer; LogHandler: TadsRTreeLogEvent);
{$else}
constructor TadsRTree.Create(MinEntries, MaxEntries: Integer);
{$endif}
begin
  {$ifdef DEBUGLOG}
  HighestInternalID := 0;
  FOnLog := LogHandler;
  {$endif}
  MinNoOfEntries := MinEntries;
  MaxNoOfEntries := MaxEntries;
  Root := CreateNode(MaxNoOfEntries, True);
end{constructor};

destructor TadsRTree.Destroy;
begin
  Clear;
  Root.Free;
  inherited;
end{destructor};

end.
