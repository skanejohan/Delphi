unit adsGeometry;

interface

{ This unit defines TadsPoint and TadsRect, which correspond to TPoint and TRect but use Double
  values for the coordinates. It also defines a number of operations on these types (many of which
  work both for TPoint/TRect and TadsPoint/TadsRect). }

uses
  Types;

type
  {** Represents a two-dimensional point. }
  TadsPoint = record
    X: Double;
    Y: Double;
  end{record};

  {** Represents a two-dimensional rectangle. Note that for many of the operations below, TopLeft.X
   must be less than or equal to BottomRight.X and TopLeft.Y must be less than or equal to
   BottomRight.Y. If necessary, use Validate() on a TadsRect object to ensure this. }
  TadsRect = record
    TopLeft: TadsPoint;
    BottomRight: TadsPoint;
  end{record};

{** Returns a TadsPoint object with given coordinates. }
function adsPoint(aX, aY: Double): TadsPoint;

{** Returns a TadsRect object with given coordinates. }
function adsRect(Left, Top, Right, Bottom: Double): TadsRect;

{** Returns a rectangle which is the union of R1 and R2, i.e. the minimal rectangle that encloses
 them both. R1 and R2 must be validated. }
function Union(R1, R2: TRect): TRect; overload;
function Union(R1, R2: TadsRect): TadsRect; overload;

{** Returns the area of R. }
function Area(R: TRect): Integer; overload;
function Area(R: TadsRect): Double; overload;

{** Ensures that R.TopLeft has lower X and Y than R.BottomRight, if needed by swapping coordinates. }
procedure Validate(var R: TRect); overload;
procedure Validate(var R: TadsRect); overload;

{** Returns True if the given rectangle has width = 0 and height = 0. }
function RectEmpty(Rect: TRect): Boolean; overload;
function RectEmpty(Rect: TadsRect): Boolean; overload;

{** Returns the bounding box of the given rectangle. Calling R := BoundingBox(R) is the same as
 calling Validate(R). }
function BoundingBox(R: TRect): TRect; overload;
function BoundingBox(R: TadsRect): TadsRect; overload;

{** Returns the bounding box of the given polygon. }
function BoundingBox(Polygon: array of TPoint): TRect; overload;
function BoundingBox(Polygon: array of TadsPoint): TadsRect; overload;

{** Returns True if R1 and R2 overlap, False otherwise. Note that R1 and R2 must be validated. }
function Overlap(R1, R2: TRect): Boolean; overload;
function Overlap(R1, R2: TadsRect): Boolean; overload;

{** Returns a value > 0 if the two points lie on the same side of the line, a value < 0 if they are
 on different sides of the line, and 0 if one of the points lies on the line. }
function OnSameSide(Line: TRect; P1, P2: TPoint): Integer; overload;
function OnSameSide(Line: TadsRect; P1, P2: TadsPoint): Double; overload;

{** Retuns True if the two lines intersect, False otherwise. }
function Intersect(Line1, Line2: TRect): Boolean; overload;
function Intersect(Line1, Line2: TadsRect): Boolean; overload;

{** Returns the point where Line1 and Line2 intersect.}
function IntersectAt(Line1, Line2: TRect): TPoint; overload;
function IntersectAt(Line1, Line2: TadsRect): TadsPoint; overload;

{** Returns True if the points lies inside the polygon. You can call this method without supplying
 the polygon's bounding box, in which case it will be calculated, or you can send along a
 precalculated bounding box, to increase the method's performance. Note that this method is not
 implemented for TadsPoint/TadsRect. }
function Inside(Point: TPoint; Polygon: array of TPoint): Boolean; overload;
function Inside(Point: TPoint; Polygon: array of TPoint; BoundingBox: TRect): Boolean; overload;

implementation

uses
  Math;

function adsPoint(aX, aY: Double): TadsPoint;
begin
  Result.X := aX;
  Result.Y := aY;
end{function};

function adsRect(Left, Top, Right, Bottom: Double): TadsRect;
begin
  Result.TopLeft.X := Left;
  Result.TopLeft.Y := Top;
  Result.BottomRight.X := Right;
  Result.BottomRight.Y := Bottom;
end{function};

function Union(R1, R2: TRect): TRect;
begin
  Result.TopLeft.X := Min(R1.TopLeft.X, R2.TopLeft.X);
  Result.TopLeft.Y := Min(R1.TopLeft.Y, R2.TopLeft.Y);
  Result.BottomRight.X := Max(R1.BottomRight.X, R2.BottomRight.X);
  Result.BottomRight.Y := Max(R1.BottomRight.Y, R2.BottomRight.Y);
end{function};

function Union(R1, R2: TadsRect): TadsRect;
begin
  Result.TopLeft.X := Min(R1.TopLeft.X, R2.TopLeft.X);
  Result.TopLeft.Y := Min(R1.TopLeft.Y, R2.TopLeft.Y);
  Result.BottomRight.X := Max(R1.BottomRight.X, R2.BottomRight.X);
  Result.BottomRight.Y := Max(R1.BottomRight.Y, R2.BottomRight.Y);
end{function};

function Area(R: TRect): Integer;
begin
  Result := Abs(R.BottomRight.X - R.TopLeft.X) * Abs(R.BottomRight.Y - R.TopLeft.Y);
end{function};

function Area(R: TadsRect): Double;
begin
  Result := Abs(R.BottomRight.X - R.TopLeft.X) * Abs(R.BottomRight.Y - R.TopLeft.Y);
end{function};

procedure Validate(var R: TRect);
begin
  R := BoundingBox(R);
end{procedure};

procedure Validate(var R: TadsRect);
begin
  R := BoundingBox(R);
end{procedure};

function RectEmpty(Rect: TRect): Boolean;
begin
  Result := (Rect.Left = Rect.Right) and (Rect.Top = Rect.Bottom);
end{function};

function RectEmpty(Rect: TadsRect): Boolean; overload;
begin
  Result := (Abs(Rect.TopLeft.X - Rect.BottomRight.X) < 1e-15) and
    (Abs(Rect.TopLeft.Y - Rect.BottomRight.Y) < 1e-15);
end{function};

function BoundingBox(R: TRect): TRect; overload;
begin
  Result.Left := Min(R.Left, R.Right);
  Result.Top := Min(R.Top, R.Bottom);
  Result.Right := Max(R.Left, R.Right);
  Result.Bottom := Max(R.Top, R.Bottom);
end{function};

function BoundingBox(R: TadsRect): TadsRect; overload;
begin
  Result.TopLeft.X := Min(R.TopLeft.X, R.BottomRight.X);
  Result.TopLeft.Y := Min(R.TopLeft.Y, R.BottomRight.Y);
  Result.BottomRight.X := Max(R.TopLeft.X, R.BottomRight.X);
  Result.BottomRight.Y := Max(R.TopLeft.Y, R.BottomRight.Y);
end{function};

function BoundingBox(Polygon: array of TPoint): TRect;
var
  i: Integer;
begin
  if High(Polygon) < Low(Polygon) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    Result.Left := Polygon[Low(Polygon)].X;
    Result.Top := Polygon[Low(Polygon)].Y;
    Result.Right := Result.Left;
    Result.Bottom := Result.Top;
    for i := Low(Polygon)+1 to High(Polygon) do
    begin
      if Result.Left > Polygon[i].X then
        Result.Left := Polygon[i].X;
      if Result.Top > Polygon[i].Y then
        Result.Top := Polygon[i].Y;
      if Result.Right < Polygon[i].X then
        Result.Right := Polygon[i].X;
      if Result.Bottom < Polygon[i].Y then
        Result.Bottom := Polygon[i].Y;
    end{for};
  end{else};
end{function};

function BoundingBox(Polygon: array of TadsPoint): TadsRect;
var
  i: Integer;
begin
  if High(Polygon) < Low(Polygon) then
    Result := adsRect(0, 0, 0, 0)
  else
  begin
    Result.TopLeft.X := Polygon[Low(Polygon)].X;
    Result.TopLeft.Y := Polygon[Low(Polygon)].Y;
    Result.BottomRight.X := Result.TopLeft.X;
    Result.BottomRight.Y := Result.TopLeft.Y;
    for i := Low(Polygon)+1 to High(Polygon) do
    begin
      if Result.TopLeft.X > Polygon[i].X then
        Result.TopLeft.X := Polygon[i].X;
      if Result.TopLeft.Y > Polygon[i].Y then
        Result.TopLeft.Y := Polygon[i].Y;
      if Result.BottomRight.X < Polygon[i].X then
        Result.BottomRight.X := Polygon[i].X;
      if Result.BottomRight.Y < Polygon[i].Y then
        Result.BottomRight.Y := Polygon[i].Y;
    end{for};
  end{else};
end{function};

function Overlap(R1, R2: TRect): Boolean; overload;
begin
  Result := not (
    (R1.Right < R2.Left) or
    (R1.Bottom < R2.Top) or
    (R2.Right < R1.Left) or
    (R2.Bottom < R1.Top)
  );
end{function};

function Overlap(R1, R2: TadsRect): Boolean; overload;
begin
  Result := not (
    (R1.BottomRight.X < R2.TopLeft.X) or
    (R1.BottomRight.Y < R2.TopLeft.Y) or
    (R2.BottomRight.X < R1.TopLeft.X) or
    (R2.BottomRight.Y < R1.TopLeft.Y)
  );
end{function};

function OnSameSide(Line: TRect; P1, P2: TPoint): Integer;
var
  dx, dy, dx1, dx2, dy1, dy2: Integer;
begin
  dx := Line.Right - Line.Left;
  dy := Line.Bottom - Line.Top;
  dx1 := P1.X - Line.Left;
  dy1 := P1.Y - Line.Top;
  dx2 := P2.X - Line.Right;
  dy2 := P2.Y - Line.Bottom;
  Result := (dx * dy1 - dy * dx1) * (dx * dy2 - dy * dx2);
end{function};

function OnSameSide(Line: TadsRect; P1, P2: TadsPoint): Double;
var
  dx, dy, dx1, dx2, dy1, dy2: Double;
begin
  dx := Line.BottomRight.X - Line.TopLeft.X;
  dy := Line.BottomRight.Y - Line.TopLeft.Y;
  dx1 := P1.X - Line.TopLeft.X;
  dy1 := P1.Y - Line.TopLeft.Y;
  dx2 := P2.X - Line.BottomRight.X;
  dy2 := P2.Y - Line.BottomRight.Y;
  Result := (dx * dy1 - dy * dx1) * (dx * dy2 - dy * dx2);
end{function};

function Intersect(Line1, Line2: TRect): Boolean;
begin
  Result := Overlap(BoundingBox(Line1), BoundingBox(Line2)) and
    (OnSameSide(Line1, Line2.TopLeft, Line2.BottomRight) <= 0) and
    (OnSameSide(Line2, Line1.TopLeft, Line1.BottomRight) <= 0);
end{function};

function Intersect(Line1, Line2: TadsRect): Boolean;
begin
  Result := Overlap(BoundingBox(Line1), BoundingBox(Line2)) and
    (OnSameSide(Line1, Line2.TopLeft, Line2.BottomRight) <= 0) and
    (OnSameSide(Line2, Line1.TopLeft, Line1.BottomRight) <= 0);
end{function};

function IntersectAt(Line1, Line2: TRect): TPoint;
var
  P: TadsPoint;
  L1, L2: TadsRect;
begin
  L1 := adsRect(Line1.Left, Line1.Top, Line1.Right, Line1.Bottom);
  L2 := adsRect(Line2.Left, Line2.Top, Line2.Right, Line2.Bottom);
  P := IntersectAt(L1, L2);
  Result.X := Trunc(P.X);
  Result.Y := Trunc(P.Y);
end;

function IntersectAt(Line1, Line2: TadsRect): TadsPoint;
var
  Line1Slope, Line2Slope: Double;
  A1, B1, C1, A2, B2, C2: Double; {Constants of linear equations}
  DI: Double; {The inverse of the determinant of the coefficient matrix}
begin
  if (Line1.BottomRight.X - Line1.TopLeft.X) <> 0 then
    Line1Slope := (Line1.BottomRight.Y - Line1.TopLeft.Y) / (Line1.BottomRight.X - Line1.TopLeft.X)
  else
    Line1Slope := 1e+10; {Large enough to work with a vertical line}
  if (Line2.BottomRight.X - Line2.TopLeft.X) <> 0 then
    Line2Slope := (Line2.BottomRight.Y - Line2.TopLeft.Y) / (Line2.BottomRight.X - Line2.TopLeft.X)
  else
    Line2Slope := 1e+10; {Large enough to work with a vertical line}
  {Compute constants for the linear equations}
  A1 := Line1Slope;
  A2 := Line2Slope;
  B1 := -1;
  B2 := -1;
  C1 := (Line1.TopLeft.Y - Line1Slope * Line1.TopLeft.X);
  C2 := (Line2.TopLeft.Y - Line2Slope * Line2.TopLeft.X);
  {Compute the inverse of the determinant}
  DI := 1 / (A1 * B2 - A2 * B1);
  {Use Kramers rule to compute intersection point}
  Result.X := (B1 * C2 - B2 * C1) * DI;
  Result.Y := (A2 * C1 - A1 * C2) * DI;
end; {class function}

function Inside(Point: TPoint; Polygon: array of TPoint; BoundingBox: TRect): Boolean;
var
  Intersections: Integer;
  TestLine, BoundaryLine, NextBoundaryLine: TRect;

  procedure CheckIntersection;
  var
    Ydiff1, Ydiff2: Smallint;
  begin
    if (TestLine.Bottom = BoundaryLine.Bottom) and (TestLine.Left < BoundaryLine.Right) then
    begin
      {The test line crosses an end node}
      Ydiff1 := BoundaryLine.Bottom - BoundaryLine.Top;
      Ydiff2 := NextBoundaryLine.Bottom - NextBoundaryLine.Top;
      if ((Ydiff1 > 0) and (Ydiff2 > 0)) or ((Ydiff1 < 0) and (Ydiff2 < 0)) then
      begin
        {The test line crosses a node between two successive lines going "in
        the same vertical direction" (e.g. a shape similar to ">" or "<"). The
        intersection count should be increased. }
        Inc(Intersections);
      end; {if}
      {If the condition above is not met, the test line either crosses a node
       between two successive lines going "in different vertical directions"
       (e.g. a shape similar to "V") or coincides with a (horizontal) boundary
       line. In either case, the intersection count should not be increased. }
    end {if}
    else if TestLine.Top = BoundaryLine.Top then
      {The test line crosses a start node - don't increase the intersection count}
    else
      {The test line doesn't cross a node; check for intersection between the lines}
      if Intersect(TestLine, BoundaryLine) then
        Inc(Intersections);
  end; {procedure}

var
  i: Integer;
begin
  if (Point.X > BoundingBox.Right) or (Point.Y > BoundingBox.Bottom) or
    (Point.X < BoundingBox.Left) or (Point.Y < BoundingBox.Top) then
      Result := False
  else
  begin
    Intersections := 0;
    TestLine.Left := Point.X;
    TestLine.Top := Point.Y;
    TestLine.Right := BoundingBox.Right + 1;
    TestLine.Bottom := Point.Y;
    for i := Low(Polygon) to High(Polygon)-2 do
    begin
      BoundaryLine.Left := Polygon[i].X;
      BoundaryLine.Top := Polygon[i].Y;
      BoundaryLine.Right := Polygon[i+1].X;
      BoundaryLine.Bottom := Polygon[i+1].Y;
      NextBoundaryLine.Left := Polygon[i+1].X;
      NextBoundaryLine.Top := Polygon[i+1].Y;
      NextBoundaryLine.Right := Polygon[i+2].X;
      NextBoundaryLine.Bottom := Polygon[i+2].Y;
      CheckIntersection;
    end; {for}

    BoundaryLine.Left := Polygon[High(Polygon)-1].X;
    BoundaryLine.Top := Polygon[High(Polygon)-1].Y;
    BoundaryLine.Right := Polygon[High(Polygon)].X;
    BoundaryLine.Bottom := Polygon[High(Polygon)].Y;
    NextBoundaryLine.Left := Polygon[High(Polygon)].X;
    NextBoundaryLine.Top := Polygon[High(Polygon)].Y;
    NextBoundaryLine.Right := Polygon[Low(Polygon)].X;
    NextBoundaryLine.Bottom := Polygon[Low(Polygon)].Y;
    CheckIntersection;

    if (Polygon[Low(Polygon)].X <> Polygon[High(Polygon)].X) or
      (Polygon[Low(Polygon)].Y <> Polygon[High(Polygon)].Y) then
    begin
      BoundaryLine.Left := Polygon[High(Polygon)].X;
      BoundaryLine.Top := Polygon[High(Polygon)].Y;
      BoundaryLine.Right := Polygon[Low(Polygon)].X;
      BoundaryLine.Bottom := Polygon[Low(Polygon)].Y;
      NextBoundaryLine.Left := Polygon[Low(Polygon)].X;
      NextBoundaryLine.Top := Polygon[Low(Polygon)].Y;
      NextBoundaryLine.Right := Polygon[Low(Polygon)+1].X;
      NextBoundaryLine.Bottom := Polygon[Low(Polygon)+1].Y;
      CheckIntersection;
    end; {if}
    Result := Intersections mod 2 = 1;
  end; {else}
end{function};

function Inside(Point: TPoint; Polygon: array of TPoint): Boolean;
begin
  Result := Inside(Point, Polygon, BoundingBox(Polygon));
end{function};

end.
