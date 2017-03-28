unit adsHeightMapClass;

{ © 2010 Aston Design Studio }

interface

uses
  Classes, SysUtils;

type
  EInvalidSide = class(Exception);
  EInvalidCoordinates = class(Exception);

  {** This class represents a height map, i.e. a matrix of integer values. Use
   a height map generator to populate the height map or set the Height property
   explicitly. }
  THeightMap = class
  private
    FSide: Integer;
    HighestValue: Integer;
    HeightCounter: Array of Integer;
    Terrain: Array of Array of Integer;
    procedure SetSide(const Value: Integer);
    function GetHeight(X, Y: Integer): Integer;
    procedure SetHeight(X, Y, Value: Integer);
    function GetBreakpoint(Percentage: Integer): Integer;
  public

    {** The length (-1) of a side in the height map. The side is indexed from 0
     to Side. Side must equal 2 raised to an integer value (i.e. 2, 4, 8, 16
     etc.) or an EInvalidSide exception is raised. Default is 256. }
    property Side: Integer read FSide write SetSide;

    {** Read this property to get the height value under which a certain
     percentage of the heights lie. Note that the Normalize method must have
     been called before this property is read. }
    property Breakpoint[Percentage: Integer]: Integer read GetBreakpoint;

    {** The height at the given (X, Y) coordinates. X and Y must both be within
     (0..Side) or an EInvalidCoordinates exception is raised. }
    property Height[X, Y: Integer]: Integer read GetHeight write SetHeight; default;

    {** Call this method to clear the height map, i.e. set all heights to 0.
     This method is also called automatically when the Side property is set. }
    procedure Clear;

    {** After a call to this method, all values are guaranteed to lie between 0
     and this value. The height map generators automatically call this method to
     enforce their MaxHeight property value. }
    procedure Normalize(MaxHeight: Integer);

    constructor Create;
  end{class};

implementation

uses
  Math;

{---------- THeightMap --------------------------------------------------------}

procedure THeightMap.SetSide(const Value: Integer);
begin
  if Trunc(Log2(Value)) <> Log2(Value) then
    Raise EInvalidSide.Create('Side must be 2 raised to an integer value');
  FSide := Value;
  Clear;
end{procedure};

function THeightMap.GetHeight(X, Y: Integer): Integer;
begin
  if (X < 0) or (X > Side) or (Y < 0) or (Y > Side)then
    Raise EInvalidCoordinates.Create(Format(
      'Coordinates (%d,%d) outside allowed range', [X, Y]));
  Result := Terrain[Y, X];
end{function};

procedure THeightMap.SetHeight(X, Y, Value: Integer);
begin
  Terrain[Y, X] := Value;
  if Value > HighestValue then
    HighestValue := Value;
end{procedure};

function THeightMap.GetBreakpoint(Percentage: Integer): Integer;
var
  i, Ctr: Integer;
begin
  Ctr := 0;
  Result := 0;
  for i := 0 to Length(HeightCounter)-1 do
  begin
    Ctr := Ctr + HeightCounter[i];
    if Ctr / (Side * Side) > Percentage / 100 then
    begin
      Result := i;
      Break;
    end{if};
  end{for};
end{function};

procedure THeightMap.Clear;
var
  x, y: Integer;
begin
  SetLength(Terrain, Side+1);
  for y := 0 to Side do
  begin
    SetLength(Terrain[y], Side+1);
    for x := 0 to Side do
      Terrain[y, x] := 0;
  end{for};
  HighestValue := -MAXINT;
end{procedure};

procedure THeightMap.Normalize(MaxHeight: Integer);
var
  x, y: Integer;
  Normalizer: Double;
begin
  SetLength(HeightCounter, MaxHeight + 1);
  for x := 0 to MaxHeight do
    HeightCounter[x] := 0;
  Normalizer := MaxHeight / HighestValue;
  for X  := 0 to Side do
    for Y := 0 to Side do
    begin
      Terrain[X, Y] := Trunc(Terrain[X, Y] * Normalizer);
      HeightCounter[Terrain[X, Y]] := HeightCounter[Terrain[X, Y]] + 1;
    end{for};
end{procedure};

constructor THeightMap.Create;
begin
  Side := 256;
end{constructor};

end.
