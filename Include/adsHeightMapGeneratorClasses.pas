unit adsHeightMapGeneratorClasses;

{ © 2010 Aston Design Studio }

interface

uses
  Contnrs, adsObjectClass, adsHeightMapClass;

type

  {** This is the base class from which all map generators inherit. }
  TAbstractHeightMapGenerator = class(TadsObject)
  private
    FMaxHeight: Integer;
  protected
    procedure PopulateHeightMap(HeightMap: THeightMap); virtual; abstract;
  public

    {** The maximum height at any given coordinate in the terrain. The value of
     a coordinate will be guaranteed to lie between 0 and this value after the
     generation. Set this before calling Generate. Default is 100. }
    property MaxHeight: Integer read FMaxHeight write FMaxHeight;

    {** Populate the given height map with values. This method will clear the
     height map, call the PopulateHeightMap method (which must be implemented
     in derived classes) and finally normalize the height map. }
    procedure Generate(HeightMap: THeightMap);

    constructor Create; virtual;
  end{class};

  {** This class implements a map generator which generates the map according to
   the midpoint displacement algorithm. }
  TMidpointDisplacementHeightMapGenerator = class(TAbstractHeightMapGenerator)
  private
    FRoughness: Double;
  protected
    procedure PopulateHeightMap(HeightMap: THeightMap); override;
  public

    {** The roughness value. The greater the number, the smoother the terrain.
     Default is 0.5. }
    property Roughness: Double read FRoughness write FRoughness;

    constructor Create; override;
  end{class};

  {** Helper class used in TCircularPeakHeightMapGenerator. }
  TPeak = class(TadsObject)
    X, Y, Height, RadiusSquared: Integer;
  end{class};

  {** This class implements a map generator which generates a map containing
   cones with centers and radii according to the ones specified using AddPeak.}
  TCircularPeakHeightMapGenerator = class(TAbstractHeightMapGenerator)
  protected
    Peaks: TObjectList; {Of TPeak}
    procedure PopulateHeightMap(HeightMap: THeightMap); override;
  public

    {** Clears the list of peaks. }
    procedure ClearPeaks;

    {** Adds a peak with given values. }
    procedure AddPeak(X, Y, Height, Radius: Integer);

    constructor Create; override;
    destructor Destroy; override;
  end{class};

implementation

uses
  Math;

{---------- TAbstractHeightMapGenerator ---------------------------------------}

procedure TAbstractHeightMapGenerator.Generate(HeightMap: THeightMap);
begin
  HeightMap.Clear;
  PopulateHeightMap(HeightMap);
  HeightMap.Normalize(FMaxHeight);
end{procedure};

constructor TAbstractHeightMapGenerator.Create;
begin
  MaxHeight := 100;
end{constructor};

{---------- TMidpointDisplacementHeightMapGenerator ---------------------------}

procedure TMidpointDisplacementHeightMapGenerator.PopulateHeightMap(HeightMap: THeightMap);
var
  D: Double;
  CurrentSide, Steps, Xdelta, Ydelta, X, Y, X1, Y1, X2, Y2: Integer;

  procedure PerformDiamondStep;
  var
    XC, YC, H: Integer;
  begin
    XC := (X2 + X1) div 2;
    YC := (Y2 + Y1) div 2;
    with HeightMap do
    begin
      H := Trunc((Height[X1, Y1] + Height[X1, Y2] + Height[X2, Y1] +
        Height[X2, Y2]) / 4 + Random(Trunc(D)));
      Height[XC, YC] := H;
    end{with};
  end{procedure};

  procedure PerformSquareStep(X, Y: Integer);
  var
    ValueUp, ValueRight, ValueDown, ValueLeft, H: Integer;
  begin
    with HeightMap do
    begin
      if X = 0 then
        ValueLeft := Height[X + Xdelta, Y]
      else
        ValueLeft := Height[X - Xdelta, Y];
      if X = side then
        ValueRight := Height[X - Xdelta, Y]
      else
        ValueRight := Height[X + Xdelta, Y];
      if Y = 0 then
        ValueDown := Height[X, Y + Ydelta]
      else
        ValueDown := Height[X, Y - Ydelta];
      if Y = side then
        ValueUp := Height[X, Y - Ydelta]
      else
        ValueUp := Height[X, Y + Ydelta];
      H := Trunc((ValueLeft + ValueUp + ValueRight + ValueDown) / 4 +
        Random(Trunc(D)));
      Height[X, Y] := H;
    end;
  end{procedure};

begin
  D := 100;
  CurrentSide := HeightMap.Side;
  while CurrentSide > 1 do
  begin
    Steps := HeightMap.Side div CurrentSide;
    Xdelta := CurrentSide div 2;
    Ydelta := CurrentSide div 2;
    for X := 0 to Steps-1 do
      for Y := 0 to Steps-1 do
      begin
        X1 := X * CurrentSide;
        Y1 := Y * CurrentSide;
        X2 := (X + 1) * CurrentSide;
        Y2 := (Y + 1) * CurrentSide;
        PerformDiamondStep;
      end; {for}
    D := D * Power(2, -Roughness);
    for X := 0 to Steps-1 do
      for Y := 0 to Steps-1 do
      begin
        X1 := X * CurrentSide;
        Y1 := Y * CurrentSide;
        X2 := (X + 1) * CurrentSide;
        Y2 := (Y + 1) * CurrentSide;
        PerformSquareStep(X1, Y1 + Ydelta);
        PerformSquareStep(X1 + Xdelta, Y2);
        PerformSquareStep(X2, Y1 + Ydelta);
        PerformSquareStep(X1 + Xdelta, Y1);
      end; {for}
    D := D * Power(2, -Roughness);
    CurrentSide := CurrentSide div 2;
  end{while};
end{procedure};

constructor TMidpointDisplacementHeightMapGenerator.Create;
begin
  inherited;
  Roughness := 0.5;
end{constructor};

{---------- TCircularPeakHeightMapGenerator -----------------------------------}

procedure TCircularPeakHeightMapGenerator.ClearPeaks;
begin
  Peaks.Clear;
end{procedure};

procedure TCircularPeakHeightMapGenerator.AddPeak(X, Y, Height, Radius: Integer);
var
  Peak: TPeak;
begin
  Peak := TPeak.Create;
  Peak.X := X;
  Peak.Y := Y;
  Peak.Height := Height;
  Peak.RadiusSquared := Radius * Radius;
  Peaks.Add(Peak);
end{procedure};

procedure TCircularPeakHeightMapGenerator.PopulateHeightMap(HeightMap: THeightMap);
var
  X, Y, Height, i: Integer;
  DistanceSquared: Integer;
begin
  for X := 0 to HeightMap.Side-1 do
    for Y := 0 to HeightMap.Side-1 do
    begin
      Height := 0;
      for i := 0 to Peaks.Count-1 do
      begin
        DistanceSquared := (TPeak(Peaks[i]).X - X) * (TPeak(Peaks[i]).X - X) +
          (TPeak(Peaks[i]).Y - Y) * (TPeak(Peaks[i]).Y - Y);
        if DistanceSquared < TPeak(Peaks[i]).RadiusSquared then
          Height := Height + Trunc(TPeak(Peaks[i]).Height * (TPeak(Peaks[i]).RadiusSquared  - DistanceSquared) /
            TPeak(Peaks[i]).RadiusSquared);
      end{for};
      HeightMap.Height[X, Y] := Height;
    end{for};
end{procedure};

constructor TCircularPeakHeightMapGenerator.Create;
begin
  inherited;
  Peaks := TObjectList.Create;
end{constructor};

destructor TCircularPeakHeightMapGenerator.Destroy;
begin
  Peaks.Free;
  inherited;
end{destructor};


end.
