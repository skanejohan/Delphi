unit adsMapControlClass;

{ © 2009 Aston Design Studio }

interface

uses
  Classes, Graphics, Controls, Types, SyncObjs, adsThreadedDrawingControlClass;

type
  TadsZoomMode = (zmIn, zmOut);
  TadsLeftButtonMode = (lbSelect, lbPan);

  TadsMapPoint = record
    X: Double;
    Y: Double;
  end{record};

  TadsMapPointEvent = procedure(Sender: TObject; MapPoint: TadsMapPoint) of object;

  {** This class is used to convert between screen and map coordinates. It is
   used by both TadsMapDrawingThread and TadsMapControl, and all public
   properties and methods are thread-safe. }
  TadsMapCoordinateCalculator = class
  private
    FScale: Double;
    Width: Integer;
    Height: Integer;
    FTopLeft: TadsMapPoint;
    FTopLeftWhenMouseDown: TadsMapPoint;
    FOffsetWhenMouseDown: TadsMapPoint;
    FBottomRight: TadsMapPoint;
    Access: TCriticalSection;
    function GetScale: Double;
    procedure SetScale(const Value: Double);
    function GetTopLeft: TadsMapPoint;
    procedure SetTopLeft(const Value: TadsMapPoint);
    function GetTopLeftWhenMouseDown: TadsMapPoint;
    procedure SetTopLeftWhenMouseDown(const Value: TadsMapPoint);
    function GetOffsetWhenMouseDown: TadsMapPoint;
    procedure SetOffsetWhenMouseDown(const Value: TadsMapPoint);
    function GetBottomRight: TadsMapPoint;
    procedure SetBottomRight(const Value: TadsMapPoint);
  protected
    property Scale: Double read GetScale write SetScale;
    property TopLeft: TadsMapPoint read GetTopLeft write SetTopLeft;
    property TopLeftWhenMouseDown: TadsMapPoint read GetTopLeftWhenMouseDown
      write SetTopLeftWhenMouseDown;
    property OffsetWhenMouseDown: TadsMapPoint read GetOffsetWhenMouseDown
      write SetOffsetWhenMouseDown;
    property BottomRight: TadsMapPoint read GetBottomRight write SetBottomRight;
    procedure SetDimensions(aWidth, aHeight: Integer);
    procedure CalculateBottomRight;
    function CanvasToMapOffset(canvasX, canvasY: Integer): TadsMapPoint;
    function CanvasToMap(canvasX, canvasY: Integer): TadsMapPoint;
    function MapToCanvas(mapX, mapY: Double): TPoint;
  public
    constructor Create;
    destructor Destroy; override;
  end{class};

  {** This class extends TadsDrawingThread with methods corresponding to similar
   methods in TCanvas, such as MoveTo, LineTo etc. It automatically handles
   conversion between screen and map coordinates. }
  TadsMapDrawingThread = class(TadsDrawingThread)
  protected
    Calculator: TadsMapCoordinateCalculator;
  public

    {** Changes the current drawing position to the position on the canvas
     corresponding to the given map coordinates (i.e. it corresponds to TCanvas.
     MoveTo, but "in map coordinates"). }
    procedure MoveTo(mapX, mapY: Double);

    {** Draws a line on the canvas from the current drawing position to the
     position on the canvas corresponding to the given map coordinates, and sets
     the current drawing position to this position (i.e. it corresponds to
     TCanvas.LineTo, but "in map coordinates"). }
    procedure LineTo(mapX, mapY: Double);

    {** Draws a polyline on the canvas using map coordinates (i.e. corresponds
     to TCanvas.Polyline, but "in map coordinates"). }
    procedure Polyline(mapPoints: Array of TadsMapPoint);

    {** Draws a polygon on the canvas using map coordinates (i.e. corresponds
     to TCanvas.Polygon, but "in map coordinates"). }
    procedure Polygon(mapPoints: Array of TadsMapPoint);

    {** Draws a point (actually a circle) on the canvas, "in map coordinates",
     with given radius (in pixels). }
    procedure Point(mapPoint: TadsMapPoint; Radius: Integer=1);

  end{class};

  {** This control represents a canvas, on which you can draw using map
   coordinates which are automatically converted to canvas coordinates. It also
   supports automatic panning (CTRL + left mouse button) and zooming (mouse
   wheel). }
  TadsMapControl = class(TadsThreadedDrawingControl)
  private
    FAutoFocus: Boolean;
    FOnBoundsChanged: TNotifyEvent;
    FOnSelect: TadsMapPointEvent;
    FOnMouseMove: TadsMapPointEvent;
    function GetBottomRight: TadsMapPoint;
    function GetTopLeft: TadsMapPoint;
  protected
    Calculator: TadsMapCoordinateCalculator;
    LeftButtonMode: TadsLeftButtonMode;
    procedure DoAfterCreateThread; override;
    procedure Zoom(aroundCanvasPoint: TPoint; zoomMode: TadsZoomMode);
  protected
    procedure DoBoundsChanged; virtual;
    procedure Resize; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public

    {** If True, the control will automatically receive focus when the mouse
     moves into it. Default is True. }
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus;

    {** The map coordinate currently positioned in the canvas's upper left
     corner. }
    property TopLeft: TadsMapPoint read GetTopLeft;

    {** The map coordinate currently positioned in the canvas's lower right
     corner. }
    property BottomRight: TadsMapPoint read GetBottomRight;

    {** Initializes the control to display the parts controlled by the given map
     coordinates. Note that it will change the coordinates so that everything
     within the coordinates is visible, maintaining the height/width ratio. The
     resulting coordinates can be read from the TopLeft and BottomRight
     properties, whose values are updated as the user zooms, pans or resizes the
     control. The OnBoundsChanged event will strike, informing you about this. }
    procedure Initialize(mapLeft, mapTop, mapRight, mapBottom: Double);

    {** Call this method to clear the canvas (i.e. fill it with the color
     specified by the Color property). Used only in un-threaded mode. }
    procedure Clear;

    {** Changes the current drawing position to the position on the canvas
     corresponding to the given map coordinates (i.e. it corresponds to TCanvas.
     MoveTo, but "in map coordinates"). Used when drawing in unthreaded mode. }
    procedure MoveTo(mapX, mapY: Double);

    {** Draws a line on the canvas from the current drawing position to the
     position on the canvas corresponding to the given map coordinates, and sets
     the current drawing position to this position (i.e. it corresponds to
     TCanvas.LineTo, but "in map coordinates"). Used when drawing in unthreaded
     mode. }
    procedure LineTo(mapX, mapY: Double);

    {** Draws a polyline on the canvas (i.e. it corresponds to TCanvas.Polyline,
     but "in map coordinates"). Used when drawing in unthreaded mode. }
    procedure Polyline(mapPoints: Array of TadsMapPoint);

    {** Draws a polygon on the canvas (i.e. it corresponds to TCanvas.Polygon,
     but "in map coordinates"). Used when drawing in unthreaded mode. }
    procedure Polygon(mapPoints: Array of TadsMapPoint);

    {** Draws a point (actually a circle) on the canvas, "in map coordinates",
     with given radius (in pixels). Used when drawing in unthreaded mode. }
    procedure Point(mapPoint: TadsMapPoint; Radius: Integer=1);

    {** Generated when TopLeft and/or BottomRight has changed. }
    property OnBoundsChanged: TNotifyEvent read FOnBoundsChanged
      write FOnBoundsChanged;

    {** Generated when the user left-clicks in the map, supplying the map
     coordinates of the click position. }
    property OnSelect: TadsMapPointEvent read FOnSelect write FOnSelect;

    {** Generated when the user moves the mouse over the map, supplying the map
     coordinates of the mouse position. }
    property OnMouseMove: TadsMapPointEvent read FOnMouseMove write FOnMouseMove;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end{class};

{** Returns a TadsMapPoint record with given parameters. }
function adsMapPoint(aX, aY: Double): TadsMapPoint;

implementation

uses
  Windows, Math, adsLoggerClass;

const
  zoomStep: Double = 1.05;

function adsMapPoint(aX, aY: Double): TadsMapPoint;
begin
  Result.X := aX;
  Result.Y := aY;
end{function};

{---------- TadsMapDrawingThread ----------------------------------------------}

procedure TadsMapDrawingThread.MoveTo(mapX, mapY: Double);
var
  CanvasPoint: TPoint;
begin
  CanvasPoint := Calculator.MapToCanvas(mapX, mapY);
  Canvas.MoveTo(CanvasPoint.X, CanvasPoint.Y);
end{procedure};

procedure TadsMapDrawingThread.LineTo(mapX, mapY: Double);
var
  CanvasPoint: TPoint;
begin
  CanvasPoint := Calculator.MapToCanvas(mapX, mapY);
  Canvas.LineTo(CanvasPoint.X, CanvasPoint.Y);
end{procedure};

procedure TadsMapDrawingThread.Polyline(mapPoints: array of TadsMapPoint);
var
  i: Integer;
  Points: Array of TPoint;
begin
  SetLength(Points, Length(mapPoints));
  for i := Low(mapPoints) to High(mapPoints) do
    Points[i] := Calculator.MapToCanvas(mapPoints[i].X, mapPoints[i].Y);
  Canvas.Polyline(Points);
end{procedure};

procedure TadsMapDrawingThread.Polygon(mapPoints: array of TadsMapPoint);
var
  i: Integer;
  Points: Array of TPoint;
begin
  SetLength(Points, Length(mapPoints));
  for i := Low(mapPoints) to High(mapPoints) do
    Points[i] := Calculator.MapToCanvas(mapPoints[i].X, mapPoints[i].Y);
  Canvas.Polygon(Points);
end{procedure};

procedure TadsMapDrawingThread.Point(mapPoint: TadsMapPoint; Radius: Integer);
var
  Point: TPoint;
begin
  Point := Calculator.MapToCanvas(mapPoint.X, mapPoint.Y);
  Canvas.Ellipse(Point.X-Radius, Point.Y-Radius, Point.X+Radius, Point.Y+Radius);
end{procedure};

{---------- TadsMapCoordinateCalculator ---------------------------------------}

function TadsMapCoordinateCalculator.GetScale: Double;
begin
  Access.Acquire;
  try
    Result := FScale;
  finally
    Access.Release;
  end{finally};
end{function};

procedure TadsMapCoordinateCalculator.SetScale(const Value: Double);
begin
  Access.Acquire;
  try
    FScale := Value;
  finally
    Access.Release;
  end{finally};
end{procedure};

function TadsMapCoordinateCalculator.GetTopLeft: TadsMapPoint;
begin
  Access.Acquire;
  try
    Result.X := FTopLeft.X;
    Result.Y := FTopLeft.Y;
  finally
    Access.Release;
  end{finally};
end{function};

procedure TadsMapCoordinateCalculator.SetTopLeft(const Value: TadsMapPoint);
begin
  Access.Acquire;
  try
    FTopLeft.X := Value.X;
    FTopLeft.Y := Value.Y;
  finally
    Access.Release;
  end{finally};
end{procedure};

function TadsMapCoordinateCalculator.GetTopLeftWhenMouseDown: TadsMapPoint;
begin
  Access.Acquire;
  try
    Result.X := FTopLeftWhenMouseDown.X;
    Result.Y := FTopLeftWhenMouseDown.Y;
  finally
    Access.Release;
  end{finally};
end{function};

procedure TadsMapCoordinateCalculator.SetTopLeftWhenMouseDown(const Value: TadsMapPoint);
begin
  Access.Acquire;
  try
    FTopLeftWhenMouseDown.X := Value.X;
    FTopLeftWhenMouseDown.Y := Value.Y;
  finally
    Access.Release;
  end{finally};
end{procedure};

function TadsMapCoordinateCalculator.GetOffsetWhenMouseDown: TadsMapPoint;
begin
  Access.Acquire;
  try
    Result.X := FOffsetWhenMouseDown.X;
    Result.Y := FOffsetWhenMouseDown.Y;
  finally
    Access.Release;
  end{finally};
end{function};

procedure TadsMapCoordinateCalculator.SetOffsetWhenMouseDown(const Value: TadsMapPoint);
begin
  Access.Acquire;
  try
    FOffsetWhenMouseDown.X := Value.X;
    FOffsetWhenMouseDown.Y := Value.Y;
  finally
    Access.Release;
  end{finally};
end{procedure};

function TadsMapCoordinateCalculator.GetBottomRight: TadsMapPoint;
begin
  Access.Acquire;
  try
    Result.X := FBottomRight.X;
    Result.Y := FBottomRight.Y;
  finally
    Access.Release;
  end{finally};
end{function};

procedure TadsMapCoordinateCalculator.SetBottomRight(const Value: TadsMapPoint);
begin
  Access.Acquire;
  try
    FBottomRight.X := Value.X;
    FBottomRight.Y := Value.Y;
  finally
    Access.Release;
  end{finally};
end{procedure};

procedure TadsMapCoordinateCalculator.SetDimensions(aWidth, aHeight: Integer);
begin
  Access.Acquire;
  try
    Width := aWidth;
    Height := aHeight;
  finally
    Access.Release;
  end{finally};
end{procedure};

procedure TadsMapCoordinateCalculator.CalculateBottomRight;
begin
  Access.Acquire;
  try
    FBottomRight.X := TopLeft.X + Scale * Width;
    FBottomRight.Y := TopLeft.Y + Scale * Height;
  finally
    Access.Release;
  end{finally};
end{procedure};

function TadsMapCoordinateCalculator.CanvasToMapOffset(canvasX, canvasY: Integer): TadsMapPoint;
begin
  Access.Acquire;
  try
    Result.X := canvasX * Scale;
    Result.Y := canvasY * Scale;
  finally
    Access.Release;
  end{finally};
end{function};

function TadsMapCoordinateCalculator.CanvasToMap(canvasX, canvasY: Integer): TadsMapPoint;
begin
  Access.Acquire;
  try
    Result := CanvasToMapOffset(canvasX, canvasY);
    Result.X := Result.X + TopLeft.X;
    Result.Y := Result.Y + TopLeft.Y;
  finally
    Access.Release;
  end{finally};
end{function};

function TadsMapCoordinateCalculator.MapToCanvas(mapX, mapY: Double): TPoint;
begin
  Access.Acquire;
  try
    Result.X := Trunc((mapX - TopLeft.X) / Scale);
    Result.Y := Trunc((mapY - TopLeft.Y) / Scale);
  finally
    Access.Release;
  end{finally};
end{function};

constructor TadsMapCoordinateCalculator.Create;
begin
  Access := TCriticalSection.Create;
end{constructor};

destructor TadsMapCoordinateCalculator.Destroy;
begin
  Access.Free;
  inherited;
end{destructor};

{---------- TadsMapControl ---------------------------------------------------}

function TadsMapControl.GetTopLeft: TadsMapPoint;
begin
  Result := Calculator.TopLeft;
end{function};

function TadsMapControl.GetBottomRight: TadsMapPoint;
begin
  Result := Calculator.BottomRight;
end{function};

procedure TadsMapControl.DoBoundsChanged;
begin
  if Assigned(OnBoundsChanged) then
    OnBoundsChanged(Self);
end{procedure};

procedure TadsMapControl.Zoom(aroundCanvasPoint: TPoint; zoomMode: TadsZoomMode);
var
  OldMapPoint, NewMapPoint: TadsMapPoint;
begin
  with Calculator do
  begin
    OldMapPoint := CanvasToMap(aroundCanvasPoint.X, aroundCanvasPoint.Y);
    case zoomMode of
      zmIn: Scale := Scale / zoomStep;
      zmOut: Scale := Scale * zoomStep;
    end{case};
    { We have now changed the scale. If we make no further adjustment, the map
      will remain "still" in the top left corner, and everything else will zoom
      in or out. To make sure that zooming is instead done around the current
      point, we need to make sure that the current canvas point still represents
      the same map point. }
    NewMapPoint := CanvasToMap(aroundCanvasPoint.X, aroundCanvasPoint.Y);
    TopLeft := adsMapPoint(TopLeft.X - NewMapPoint.X + OldMapPoint.X,
      TopLeft.Y - NewMapPoint.Y + OldMapPoint.Y);
    CalculateBottomRight;
  end{with};
  DoBoundsChanged;
end{procedure};

procedure TadsMapControl.Resize;
begin
  inherited;
  Calculator.CalculateBottomRight;
  DoBoundsChanged;
end{procedure};

procedure TadsMapControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_CONTROL then
  begin
    LeftButtonMode := lbSelect;
    Cursor := crDefault;
  end{if};
  inherited;
end{procedure};

procedure TadsMapControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_CONTROL then
  begin
    LeftButtonMode := lbPan;
    Cursor := crHandPoint;
  end{if};
  inherited;
end{procedure};

procedure TadsMapControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  offset: TadsMapPoint;
  mapPoint: TadsMapPoint;
begin
  if FAutoFocus and not Focused then
    SetFocus;
  offset := Calculator.CanvasToMapOffset(X, Y);
  if ssLeft in Shift then
  begin
    case LeftButtonMode of
      lbSelect:
        ; {Possibly used in future for rectangle selection}
      lbPan:
      begin
        with Calculator do
        begin
          TopLeft := adsMapPoint(
            TopLeftWhenMouseDown.X - offset.X + OffsetWhenMouseDown.X,
            TopLeftWhenMouseDown.Y - offset.Y + OffsetWhenMouseDown.Y);
          CalculateBottomRight;
        end{with};
        DoBoundsChanged;
      end{lbPan};
    end{case};
  end{if};
  if Assigned(OnMouseMove) then
  begin
    mapPoint.X := Calculator.TopLeft.X + offset.X;
    mapPoint.Y := Calculator.TopLeft.Y + offset.Y;
    OnMouseMove(Self, mapPoint);
  end{if};
  inherited;
end{procedure};

procedure TadsMapControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  with Calculator do
  begin
    TopLeftWhenMouseDown := TopLeft;
    OffsetWhenMouseDown := CanvasToMapOffset(X, Y);
  end{with};
  inherited;
end{procedure};

procedure TadsMapControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (LeftButtonMode = lbSelect) and Assigned(OnSelect) then
    OnSelect(Self, Calculator.CanvasToMap(X, Y));
  inherited;
end{procedure};

function TadsMapControl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Zoom(ScreenToClient(MousePos), zmOut);
  Result := inherited DoMouseWheelDown(Shift, MousePos);
end{function};

function TadsMapControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Zoom(ScreenToClient(MousePos), zmIn);
  Result := inherited DoMouseWheelUp(Shift, MousePos);
end{function};

procedure TadsMapControl.Initialize(mapLeft, mapTop, mapRight, mapBottom: Double);
var
  xScale, yScale: Double;
begin
  Calculator.TopLeft := adsMapPoint(mapLeft, mapTop);
  xScale := (mapRight - mapLeft) / Width;
  yScale := (mapBottom - mapTop) / Height;
  Calculator.Scale := Max(xScale, yScale);
  Calculator.CalculateBottomRight;
  DoBoundsChanged;
end{procedure};

procedure TadsMapControl.Clear;
var
  OldBrush: TBrush;
begin
  OldBrush := TBrush.Create;
  try
    with Canvas do
    begin
      OldBrush.Assign(Brush);
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(Rect(0, 0, Width, Height));
      Brush.Assign(OldBrush);
    end{with};
  finally
    OldBrush.Free;
  end{finally};
end{procedure};

procedure TadsMapControl.MoveTo(mapX, mapY: Double);
var
  CanvasPoint: TPoint;
begin
  CanvasPoint := Calculator.MapToCanvas(mapX, mapY);
  Canvas.MoveTo(CanvasPoint.X, CanvasPoint.Y);
end{procedure};

procedure TadsMapControl.LineTo(mapX, mapY: Double);
var
  CanvasPoint: TPoint;
begin
  CanvasPoint := Calculator.MapToCanvas(mapX, mapY);
  Canvas.LineTo(CanvasPoint.X, CanvasPoint.Y);
end{procedure};

procedure TadsMapControl.Polyline(mapPoints: Array of TadsMapPoint);
var
  i: Integer;
  Points: Array of TPoint;
begin
  SetLength(Points, Length(mapPoints));
  for i := Low(mapPoints) to High(mapPoints) do
    Points[i] := Calculator.MapToCanvas(mapPoints[i].X, mapPoints[i].Y);
  Canvas.Polyline(Points);
end{procedure};

procedure TadsMapControl.Polygon(mapPoints: Array of TadsMapPoint);
var
  i: Integer;
  Points: Array of TPoint;
begin
  SetLength(Points, Length(mapPoints));
  for i := Low(mapPoints) to High(mapPoints) do
    Points[i] := Calculator.MapToCanvas(mapPoints[i].X, mapPoints[i].Y);
  Canvas.Polygon(Points);
end{procedure};

procedure TadsMapControl.Point(mapPoint: TadsMapPoint; Radius: Integer=1);
var
  Point: TPoint;
begin
  Point := Calculator.MapToCanvas(mapPoint.X, mapPoint.Y);
  Canvas.Ellipse(Point.X-Radius, Point.Y-Radius, Point.X+Radius, Point.Y+Radius);
end{procedure};

procedure TadsMapControl.DoAfterCreateThread;
begin
  inherited;
  (DrawingThread as TadsMapDrawingThread).Calculator := Calculator;
end{procedure};

constructor TadsMapControl.Create(aOwner: TComponent);
begin
  inherited;
  FAutoFocus := True;
  TabStop := True;
  LeftButtonMode := lbSelect;
  DoubleBuffered := True;
  DrawingThreadClass := TadsMapDrawingThread;
  Calculator := TadsMapCoordinateCalculator.Create;
end{constructor};

destructor TadsMapControl.Destroy;
begin
  Calculator.Free;
  inherited;
end{destructor};

end.
