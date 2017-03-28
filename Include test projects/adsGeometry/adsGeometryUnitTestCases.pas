unit adsGeometryUnitTestCases;

interface

uses
  TestFrameWork, Types, adsGeometry;

type
  TadsGeometryTestCase = class(TTestCase)
  protected
    P1, P2: TPoint;
    R1, R2: TRect;
    AP1, AP2: TadsPoint;
    AR1, AR2: TadsRect;
    procedure SetUp; override;
  published
    procedure TestAdsPoint;
    procedure TestAdsRect;
    procedure TestUnion;
    procedure TestArea;
    procedure TestValidate; {Implicitly tests also BoundingBox for TRect and TadsRect}
    procedure TestRectEmpty;
    procedure TestPolygonBoundingBox;
    procedure TestOverlap;
    {procedure TestOnSameSide; This is tested in the visual test application}
    {procedure TestIntersect; This is tested in the visual test application}
    {procedure TestIntersectAt; This is tested in the visual test application}
    {procedure TestInside; This is tested in the visual test application}
  end{class};

implementation

uses
  Classes, Forms, SysUtils;

procedure TadsGeometryTestCase.SetUp;
begin
  P1 := Point(10, 20);
  P2 := Point(30, 40);
  R1 := Rect(15, 25, 35, 45);
  R2 := Rect(55, 65, 75, 85);
  AP1 := adsPoint(10.5, 20.5);
  AP2 := adsPoint(30.5, 40.5);
  AR1 := adsRect(15.5, 25.5, 35.5, 45.5);
  AR2 := adsRect(55.5, 65.5, 75.5, 85.5);
end;

procedure TadsGeometryTestCase.TestAdsPoint;
begin
  CheckEquals(10.5, AP1.X, 1e-15);
  CheckEquals(20.5, AP1.Y, 1e-15);
end;

procedure TadsGeometryTestCase.TestAdsRect;
begin
  CheckEquals(15.5, AR1.TopLeft.X, 1e-15);
  CheckEquals(25.5, AR1.TopLeft.Y, 1e-15);
  CheckEquals(35.5, AR1.BottomRight.X, 1e-15);
  CheckEquals(45.5, AR1.BottomRight.Y, 1e-15);
end;

procedure TadsGeometryTestCase.TestUnion;
var
  R: TRect;
  AR: TadsRect;
begin
  R := Union(R1, R2);
  CheckEquals(15, R.TopLeft.X);
  CheckEquals(25, R.TopLeft.Y);
  CheckEquals(75, R.BottomRight.X);
  CheckEquals(85, R.BottomRight.Y);

  AR := Union(AR1, AR2);
  CheckEquals(15.5, AR.TopLeft.X, 1e-15);
  CheckEquals(25.5, AR.TopLeft.Y, 1e-15);
  CheckEquals(75.5, AR.BottomRight.X, 1e-15);
  CheckEquals(85.5, AR.BottomRight.Y, 1e-15);
end;

procedure TadsGeometryTestCase.TestArea;
begin
  CheckEquals(400, Area(R1));
  CheckEquals(400, Area(AR1), 1e-15);
end;

procedure TadsGeometryTestCase.TestValidate;
var
  R: TRect;
  AR: TadsRect;
begin
  R := Rect(50, 50, 20, 20);
  AR := adsRect(50.0, 50.0, 20.0, 20.0);

  Validate(R);
  CheckEquals(20, R.TopLeft.X);
  CheckEquals(20, R.TopLeft.Y);
  CheckEquals(50, R.BottomRight.X);
  CheckEquals(50, R.BottomRight.Y);

  Validate(AR);
  CheckEquals(20.0, AR.TopLeft.X, 1e-15);
  CheckEquals(20.0, AR.TopLeft.Y, 1e-15);
  CheckEquals(50.0, AR.BottomRight.X, 1e-15);
  CheckEquals(50.0, AR.BottomRight.Y, 1e-15);
end;

procedure TadsGeometryTestCase.TestRectEmpty;
begin
  CheckEquals(True, RectEmpty(Rect(10, 20, 10, 20)));
  CheckEquals(False, RectEmpty(Rect(10, 20, 11, 21)));
  CheckEquals(True, RectEmpty(adsRect(10.0, 20.0, 10.0, 20.0)));
  CheckEquals(False, RectEmpty(adsRect(10.0, 20.0, 11.0, 21.0)));
end;

procedure TadsGeometryTestCase.TestPolygonBoundingBox;
var
  Rect: TRect;
  Polygon: Array[0..3] of TPoint;
  adsRect: TadsRect;
  adsPolygon: Array[0..3] of TadsPoint;
begin
  Polygon[0].X := 10;
  Polygon[0].Y := 10;
  Polygon[1].X := 30;
  Polygon[1].Y := 10;
  Polygon[2].X := 30;
  Polygon[2].Y := 20;
  Polygon[3].X := 5;
  Polygon[3].Y := 15;
  Rect := BoundingBox(Polygon);
  CheckEquals(5, Rect.Left);
  CheckEquals(10, Rect.Top);
  CheckEquals(30, Rect.Right);
  CheckEquals(20, Rect.Bottom);

  adsPolygon[0].X := 10;
  adsPolygon[0].Y := 10;
  adsPolygon[1].X := 30;
  adsPolygon[1].Y := 10;
  adsPolygon[2].X := 30;
  adsPolygon[2].Y := 20;
  adsPolygon[3].X := 5;
  adsPolygon[3].Y := 15;
  adsRect := BoundingBox(adsPolygon);
  CheckEquals(5, adsRect.TopLeft.X);
  CheckEquals(10, adsRect.TopLeft.Y);
  CheckEquals(30, adsRect.BottomRight.X);
  CheckEquals(20, adsRect.BottomRight.Y);
end;

procedure TadsGeometryTestCase.TestOverlap;
begin
  CheckFalse(Overlap(R1, R2));
  R2.Left := 25;
  R2.Top := 35;
  CheckTrue(Overlap(R1, R2));

  CheckFalse(Overlap(AR1, AR2));
  AR2.TopLeft.X := 25.5;
  AR2.TopLeft.Y := 35.5;
  CheckTrue(Overlap(AR1, AR2));
end;

initialization
  TestFramework.RegisterTest(TadsGeometryTestCase.Suite);

end.
