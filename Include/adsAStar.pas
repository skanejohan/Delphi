unit adsAStar;

{
  © 2007 Aston Design Studio

  Implementation of the "A Star" algorithm for finding the shortest path between
  two objects.
}

interface

uses
  Classes,
  adsContainers;

type

  {** Type for the OnOpenObject and OnClosedObject events of TadsAStar. }
  TOpenOrClosedObjectEvent = procedure(Sender, Obj: TObject) of object;

  {** Type for the OnIsObstacle event of TadsAStar. }
  TIsObstacleEvent = function(Sender, Obj: TObject): Boolean of object;

  {** Type for the OnCalculateHeuristic event of TadsAStar. }
  TCalculateHeuristicEvent = function(Sender, Obj: TObject): Integer of object;

  {** Type for the OnGetAdjacentObjects event of TadsAStar. }
  TGetAdjacentObjectsEvent = procedure(Sender, Obj: TObject; const Objs: TList;
    const Costs: TadsIntegerList) of object;

  TadsAStar = class
  private
    FOnOpenObject: TOpenOrClosedObjectEvent;
    FOnClosedObject: TOpenOrClosedObjectEvent;
    FOnIsObstacle: TIsObstacleEvent;
    FOnCalculateHeuristic: TCalculateHeuristicEvent;
    FOnGetAdjacentObjects: TGetAdjacentObjectsEvent;
  protected
    OpenNodesByScore: TadsSortedList;
    OpenNodesByObject: TadsSortedList;
    ClosedNodesByObject: TadsSortedList;
    procedure DoOpenObject(Obj: TObject);
    procedure DoClosedObject(Obj: TObject);
    function DoIsObstacle(Obj: TObject): Boolean;
    function DoCalculateHeuristic(Obj: TObject): Integer;
    procedure DoGetAdjacentObjects(Obj: TObject; const Objs: TList;
      const Costs: TadsIntegerList);
  public

    {** Generated when an object is placed on the Open list. Connect to this
     event if you need to react on this. The algorithm itself does not need you
     to do anything in this event.
     @param Sender is the TadsAStar object that generated the event.
     @param Obj This is the object that is placed on the list. }
    property OnOpenObject: TOpenOrClosedObjectEvent read FOnOpenObject
      write FOnOpenObject;

    {** Generated when an object is placed on the Closed list. Connect to this
     event if you need to react on this. The algorithm itself does not need you
     to do anything in this event.
     @param Sender is the TadsAStar object that generated the event.
     @param Obj This is the object that is placed on the list. }
    property OnClosedObject: TOpenOrClosedObjectEvent read FOnClosedObject
      write FOnClosedObject;

    {** Generated when the algorithm need you to tell it whether an object
     represents an obstacle (i.e. the path cannot pass through it) or not.
     @param Sender is the TadsAStar object that generated the event.
     @param Obj This is the object for which the function needs to know if it
      represents an obstacle.
     @return Return True if the object is an obstacle, False otherwise. }
    property OnIsObstacle: TIsObstacleEvent read FOnIsObstacle write FOnIsObstacle;

    {** This event is generated when the algorithm needs you to calculate a
     heuristic, i.e. an estimate of the cost, for going from the node to the
     path's end node.
     @param Sender is the TadsAStar object that generated the event.
     @param Obj This is the object for which the function needs to know the
      heuristic.
     @return The calculated heuristic. }
    property OnCalculateHeuristic: TCalculateHeuristicEvent
      read FOnCalculateHeuristic write FOnCalculateHeuristic;

    {** This event is generated when the algorithm needs you to inform it about
     all objects adjacent to the given one, i.e. all "neighbour objects".
     @param Sender is the TadsAStar object that generated the event.
     @param Obj This is the object for which the function needs you to calculate
      its neighbours.
     @param Objs You need to add all neighbour objects to this list.
     @param Costs You need to add the cost associated with each neighbour object
      to this list. The costs should be added in the same order as the objects
      are added to Objs, so that the value at index i in Costs represents the
      cost for the object at index i in Objs. }
    property OnGetAdjacentObjects: TGetAdjacentObjectsEvent
      read FOnGetAdjacentObjects write FOnGetAdjacentObjects;

    {** Call this method to find the shortest path from Start to Stop.
     @param The start object.
     @param The goal object.
     @param Objs All objects along the shortest path (including Start and Stop)
      will be placed in this list if a path is found.
     @returns True if a path is found, False if no path exists. }
    function FindPath(Start, Stop: TObject; const Objs: TList): Boolean;

    constructor Create;
    destructor Destroy; override;
  end; {class}

implementation

{---------- TNode and compare functions for nodes -----------------------------}

type
  TNode = class
    Cost: Integer;
    Heuristic: Integer;
    Score: Integer;
    Obj: TObject;
    Parent: TNode;
    constructor Create(aObj: TObject; aCost, aHeuristic: Integer; aParent: TNode);
  end; {class}

constructor TNode.Create(aObj: TObject; aCost, aHeuristic: Integer; aParent: TNode);
begin
  Cost := aCost;
  Heuristic := aHeuristic;
  Score := Cost + Heuristic;
  Obj := aObj;
  Parent := aParent;
end; {constructor}

function CompareNodesByScore(Item1, Item2: Pointer): Integer;
var
  Node1: TNode absolute Item1;
  Node2: TNode absolute Item2;
begin
  if Node1.Score < Node2.Score then
    Result := -1
  else if Node1.Score > Node2.Score then
    Result := 1
  else
    Result := 0;
end; {function}

function CompareNodesByObject(Item1, Item2: Pointer): Integer;
var
  Node1: TNode absolute Item1;
  Node2: TNode absolute Item2;
begin
  if Integer(Node1.Obj) < Integer(Node2.Obj) then
    Result := -1
  else if Integer(Node1.Obj) > Integer(Node2.Obj) then
    Result := 1
  else
    Result := 0;
end; {function}

{---------- TadsAStar ---------------------------------------------------------}

procedure TadsAStar.DoClosedObject(Obj: TObject);
begin
  if Assigned(OnClosedObject) then
    OnClosedObject(Self, Obj);
end; {procedure}

procedure TadsAStar.DoOpenObject(Obj: TObject);
begin
  if Assigned(OnOpenObject) then
    OnOpenObject(Self, Obj);
end; {procedure}

function TadsAStar.DoIsObstacle(Obj: TObject): Boolean;
begin
  if Assigned(OnIsObstacle) then
    Result := OnIsObstacle(Self, Obj)
  else
    Result := False;
end; {function}

function TadsAStar.DoCalculateHeuristic(Obj: TObject): Integer;
begin
  if Assigned(OnCalculateHeuristic) then
    Result := OnCalculateHeuristic(Self, Obj)
  else
    Result := 1;
end; {function}

procedure TadsAStar.DoGetAdjacentObjects(Obj: TObject; const Objs: TList;
  const Costs: TadsIntegerList);
begin
  Objs.Clear;
  Costs.Clear;
  if Assigned(OnGetAdjacentObjects) then
    OnGetAdjacentObjects(Self, Obj, Objs, Costs);
end; {function}

function TadsAStar.FindPath(Start, Stop: TObject; const Objs: TList): Boolean;
var
  i: Integer;
  AdjacentObjs: TList;
  AdjacentCosts: TadsIntegerList;
  Node, AdjacentNode, SearchNode: TNode;
begin
  AdjacentObjs := TList.Create;
  AdjacentCosts := TadsIntegerList.Create;
  try
    Result := False;
    Objs.Clear;
    Node := TNode.Create(Start, 0, 0, nil);
    OpenNodesByScore.AddSorted(Node, CompareNodesByScore);
    OpenNodesByObject.AddSorted(Node, CompareNodesByObject);
    DoOpenObject(Node.Obj);
    while OpenNodesByScore.Count > 0 do
    begin
      Node := OpenNodesByScore[0];
      OpenNodesByScore.Remove(Node);
      OpenNodesByObject.Remove(Node);
      if Node.Obj = Stop then
      begin
        {We have reached the end node. Now follow the parents back to the
         original node to create the Nodes list.}
        Objs.Add(Node.Obj);
        while (Node.Obj <> Start) and (Node.Parent <> nil) do
        begin
          Node := Node.Parent;
          Objs.Insert(0, Node.Obj);
        end; {while}
        Result := True;
        Break;
      end {if}
      else
      begin
        ClosedNodesByObject.AddSorted(Node, CompareNodesByObject);
        DoClosedObject(Node.Obj);
        DoGetAdjacentObjects(Node.Obj, AdjacentObjs, AdjacentCosts);
        for i := 0 to AdjacentObjs.Count-1 do
        begin
          SearchNode := TNode.Create(AdjacentObjs[i], 0, 0, nil);
          try
            if OpenNodesByObject.IndexOfSorted(SearchNode, CompareNodesByObject) = -1 then
              if ClosedNodesByObject.IndexOfSorted(SearchNode, CompareNodesByObject) = -1 then
                if not DoIsObstacle(AdjacentObjs[i]) then
                begin
                  AdjacentNode := TNode.Create(AdjacentObjs[i], Node.Cost +
                    AdjacentCosts[i], DoCalculateHeuristic(AdjacentObjs[i]), Node);
                  OpenNodesByScore.AddSorted(AdjacentNode, CompareNodesByScore);
                  OpenNodesByObject.AddSorted(AdjacentNode, CompareNodesByObject);
                  DoOpenObject(AdjacentNode.Obj);
                end; {if}
          finally
            SearchNode.Free;
          end; {finally}
        end; {for}
      end; {else}
    end; {while}
  finally
    OpenNodesByScore.Clear;
    OpenNodesByObject.ClearObjects;
    ClosedNodesByObject.ClearObjects;
    AdjacentCosts.Free;
    AdjacentObjs.Free;
  end; {finally}
end; {function}

constructor TadsAStar.Create;
begin
  OpenNodesByScore := TadsSortedList.Create;
  OpenNodesByObject := TadsSortedList.Create;
  ClosedNodesByObject := TadsSortedList.Create;
end; {constructor}

destructor TadsAStar.Destroy; // blaha
begin
  ClosedNodesByObject.Free;
  OpenNodesByObject.Free;
  OpenNodesByScore.Free;
  inherited;
end; {destructor}

end.
