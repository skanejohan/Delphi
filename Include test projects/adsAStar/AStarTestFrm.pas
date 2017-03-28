unit AStarTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, adsAStar, adsContainers;

type
  TAStarStatus = (asUnhandled, asOpen, asClosed, asInPath);
  TClickMode = (cmSetPredator, cmSetPrey);

  TNode = class
    Blocked: Boolean;
    AStarStatus: TAStarStatus;
    X, Y, TerrainCost: Integer;
  end; {class}
  PNode = ^TNode;

  TAStarTestForm = class(TForm)
    MapGrid: TStringGrid;
    GridTypeComboBox: TComboBox;
    procedure GridTypeComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure MapGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  protected
    Path: TList;
    AStar: TadsAStar;
    ClickMode: TClickMode;
    PredatorNode, PreyNode: TNode;
    Nodes: Array[0..19, 0..19] of TNode;
    procedure Pause;
    procedure ReadGridSettings(Grid: String);
    procedure OpenObject(Sender, Obj: TObject);
    procedure ClosedObject(Sender, Obj: TObject);
    function IsObstacle(Sender, Obj: TObject): Boolean;
    function CalculateHeuristic(Sender, Obj: TObject): Integer;
    procedure GetAdjacentObjects(Sender, Obj: TObject; const Objs: TList;
      const Costs: TadsIntegerList);
  end;

var
  AStarTestForm: TAStarTestForm;

const
  EmptyGrid =
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ';
  GridWithBlocks =
    '++                  ' +
    '+                   ' +
    '                    ' +
    '   +++++            ' +
    '       +            ' +
    '       +            ' +
    '       +            ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ' +
    '                    ';
  Labyrinth =
    '++++++++++++++++++++' +
    '+                  +' +
    '+ ++++++++++++ +++++' +
    '+ +          + + + +' +
    '+ +  +++++++ + + + +' +
    '+ ++++     +   +   +' +
    '+ +  +     +++++   +' +
    '+ +  +             +' +
    '+ +  + +++++++++++ +' +
    '+ +  + +         + +' +
    '+ +  + +         + +' +
    '+ +  + +++++++++++ +' +
    '+ +  +             +' +
    '+ +  +++++++++++++ +' +
    '+ +              + +' +
    '+ +  +           + +' +
    '+ +  +           + +' +
    '+ ++++ +++++++++++ +' +
    '+                  +' +
    '++++++++++++++++++++';
  EmptyGridWithMarshLand =
    '                    ' +
    '    ###             ' +
    '   #######          ' +
    '  ############      ' +
    '     #####          ' +
    '       ##           ' +
    '                    ' +
    '                    ' +
    '              ###   ' +
    '               ###  ' +
    '                ##  ' +
    '                ##  ' +
    '                ##  ' +
    '     ####      ##   ' +
    '   #####     ###    ' +
    '   ####     ###     ' +
    '           ####     ' +
    '            ####    ' +
    '              ###   ' +
    '                    ';

implementation

{$R *.dfm}

uses
  Math, adsLoggerClass;

procedure TAStarTestForm.GridTypeComboBoxChange(Sender: TObject);
begin
  case GridTypeComboBox.ItemIndex of
    0: ReadGridSettings(EmptyGrid);
    1: ReadGridSettings(GridWithBlocks);
    2: ReadGridSettings(Labyrinth);
    3: ReadGridSettings(EmptyGridWithMarshLand);
  end; {case}
end; {procedure}

procedure TAStarTestForm.ReadGridSettings(Grid: String);
var
  x, y: Integer;
begin
  for x := 0 to 19 do
    for y := 0 to 19 do
    begin
      MapGrid.Cells[x,y] := Grid[y*20+x+1];
      Nodes[x,y].x := x;
      Nodes[x,y].y := y;
      Nodes[x,y].AStarStatus := asUnhandled;
      if Grid[y*20+x+1] = ' ' then
      begin
        Nodes[x,y].Blocked := False;
        Nodes[x,y].TerrainCost := 1;
      end; {if}
      if Grid[y*20+x+1] = '#' then
      begin
        Nodes[x,y].Blocked := False;
        Nodes[x,y].TerrainCost := 3;
      end; {if}
      if Grid[y*20+x+1] = '+' then
      begin
        Nodes[x,y].Blocked := True;
        Nodes[x,y].TerrainCost := 0;
      end; {if}
    end; {for}
end; {procedure}

procedure TAStarTestForm.MapGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  i: Integer;
begin
  Path.Clear;
  case ClickMode of
    cmSetPredator:
    begin
      MapGrid.Cells[ACol, ARow] := '1';
      PredatorNode := Nodes[ACol, ARow];
      ClickMode := cmSetPrey;
    end; {cmSetPredator}
    cmSetPrey:
    begin
      MapGrid.Cells[ACol, ARow] := '2';
      PreyNode := Nodes[ACol, ARow];
      if AStar.FindPath(PredatorNode, PreyNode, Path) then
        for i := 0 to Path.Count-1 do
          Nodes[TNode(Path[i]).X, TNode(Path[i]).Y].AStarStatus := asInPath
      else
        ShowMessage('No path found');
      ClickMode := cmSetPredator;
      MapGrid.Repaint;
    end; {cmSetPrey}
  end; {case}
end; {procedure}

procedure TAStarTestForm.Pause;
var
  EndTick: Cardinal;
begin
  EndTick := GetTickCount + 100;
  while GetTickCount < EndTick do
    Application.ProcessMessages;
end;

procedure TAStarTestForm.ClosedObject(Sender, Obj: TObject);
var
  Node: TNode absolute Obj;
begin
  MapGrid.Cells[Node.X, Node.Y] := MapGrid.Cells[Node.X, Node.Y];
  Nodes[Node.X, Node.Y].AStarStatus := asClosed;
  Logger.Log(Self, ltDebug, Format('Node at (%d,%d) closed', [Node.X, Node.Y]));
end; {procedure}

procedure TAStarTestForm.OpenObject(Sender, Obj: TObject);
var
  Node: TNode absolute Obj;
begin
  Nodes[Node.X, Node.Y].AStarStatus := asOpen;
  Logger.Log(Self, ltDebug, Format('Node at (%d,%d) opened', [Node.X, Node.Y]));
end; {procedure}

function TAStarTestForm.CalculateHeuristic(Sender, Obj: TObject): Integer;
var
  Node: TNode absolute Obj;
begin
  {The different heuristic functions give slightly different results}
  Result := Max(Abs(PreyNode.X - Node.X), Abs(PreyNode.Y - Node.Y));
  {Result := Round(Sqrt(Power(Abs(PreyNode.X - Node.X), 2) +
    Power(Abs(PreyNode.Y - Node.Y), 2)));}
  {Result := Abs(PreyNode.X - Node.X) + Abs(PreyNode.Y - Node.Y);}
end; {function}

procedure TAStarTestForm.GetAdjacentObjects(Sender, Obj: TObject;
  const Objs: TList; const Costs: TadsIntegerList);
var
  Node: TNode absolute Obj;

  procedure AddAt(X, Y, ExtraCost: Integer);
  begin
    if (X > -1) and (X < 20) and (Y > -1) and (Y < 20) then
      if not Nodes[X, Y].Blocked then
      begin
        Objs.Add(Nodes[X, Y]);
        Costs.Add(Nodes[X, Y].TerrainCost + ExtraCost);
      end; {if}
  end; {procedure}

begin
  AddAt(Node.X-1, Node.Y-1, 2);
  AddAt(Node.X, Node.Y-1, 0);
  AddAt(Node.X+1, Node.Y-1, 2);
  AddAt(Node.X+1, Node.Y, 0);
  AddAt(Node.X+1, Node.Y+1, 2);
  AddAt(Node.X, Node.Y+1, 0);
  AddAt(Node.X-1, Node.Y+1, 2);
  AddAt(Node.X-1, Node.Y, 0);
end; {procedure}

function TAStarTestForm.IsObstacle(Sender, Obj: TObject): Boolean;
var
  Node: TNode absolute Obj;
begin
  Result := Node.Blocked;
end; {function}

procedure TAStarTestForm.MapGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with (Sender as TStringGrid) do
  begin
    if Nodes[ACol, ARow].Blocked then
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := clBlack;
    end {else}
    else
    begin
      case Nodes[ACol, ARow].AStarStatus of
        asUnhandled:
        begin
          Canvas.Brush.Color := clWhite;
          Canvas.Pen.Color := clBlack;
        end; {asUnhandled}
        asOpen:
        begin
          Canvas.Brush.Color := clYellow;
          Canvas.Pen.Color := clBlack;
        end; {asOpen}
        asClosed:
        begin
          Canvas.Brush.Color := clRed;
          Canvas.Pen.Color := clWhite;
        end; {asClosed}
        asInPath:
        begin
          Canvas.Brush.Color := clGreen;
          Canvas.Pen.Color := clWhite;
        end; {asUnhandled}
      end; {case}
    end; {else}
    Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, Cells[ACol, ARow]);
    Canvas.FrameRect(Rect);
  end;
end;

procedure TAStarTestForm.FormCreate(Sender: TObject);
var
  x, y: Integer;
begin
  Path := TList.Create;
  AStar := TadsAStar.Create;
  AStar.OnOpenObject := OpenObject;
  AStar.OnClosedObject := ClosedObject;
  AStar.OnIsObstacle := IsObstacle;
  AStar.OnCalculateHeuristic := CalculateHeuristic;
  AStar.OnGetAdjacentObjects := GetAdjacentObjects;
  for x := 0 to 19 do
    for y := 0 to 19 do
      Nodes[x, y] := TNode.Create;
  GridTypeComboBoxChange(nil);
  ClickMode := cmSetPredator;
  Logger.AddSubscription(Application.ExeName + '.log');
end; {procedure}

end.
