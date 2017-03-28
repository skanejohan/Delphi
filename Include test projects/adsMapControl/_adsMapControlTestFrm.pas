unit _adsMapControlTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, adsMapControlClass, adsThreadedDrawingControlClass, StdCtrls;

type
  TadsMapControlTestForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  protected
    MapControl: TadsMapControl;
    procedure MapControlUnthreadedDraw;
    procedure MapControlThreadedDraw(Sender: TadsDrawingThread);
    procedure MapControlBoundsChanged(Sender: TObject);
    procedure MapControlSelect(Sender: TObject; MapPoint: TadsMapPoint);
    procedure MapControlMouseMove(Sender: TObject; MapPoint: TadsMapPoint);
  end;

var
  adsMapControlTestForm: TadsMapControlTestForm;

implementation

{$R *.dfm}

uses
  adsLoggerClass;

const
  centerX: Integer = 12500; // X origin of the spiral.
  centerY: Integer = 1250;  // Y origin of the spiral.
  radius: Integer = 5000;   // Distance from origin to outer arm.
  sides: Integer = 20000;   // Number of points or sides along the spiral's arm.
  coils: Integer = 400;     // Number of coils or full rotations. (Positive numbers spin clockwise, negative numbers spin counter-clockwise)
  rotation: Double = 0;     // Overall rotation of the spiral. ('0'=no rotation, '1'=360 degrees, '180/360'=180 degrees)

procedure TadsMapControlTestForm.MapControlBoundsChanged(Sender: TObject);
begin
  if MapControl.Threaded then
    MapControl.StartDrawing
  else
    MapControlUnthreadedDraw;
  Label2.Caption := Format('(%.2f,%.2f) - (%.2f,%.2f)', [MapControl.TopLeft.X,
    MapControl.TopLeft.Y, MapControl.BottomRight.X, MapControl.BottomRight.Y]);
end;

procedure TadsMapControlTestForm.MapControlSelect(Sender: TObject;
  MapPoint: TadsMapPoint);
begin
  Label1.Caption := Format('SELECT: %.6f,%.6f', [MapPoint.X, MapPoint.Y]);
end;

procedure TadsMapControlTestForm.MapControlMouseMove(Sender: TObject;
  MapPoint: TadsMapPoint);
begin
  Caption := Format('%.6f,%.6f', [MapPoint.X, MapPoint.Y]);
end;

procedure TadsMapControlTestForm.MapControlThreadedDraw(Sender: TadsDrawingThread);
var
  i, x, y: Integer;
  Cancel: Boolean;
  away: Double;
  around: Double;
  awayStep: Double;
  aroundStep: Double;
  aroundRadians: Double;
  rotationRadians: Double;
begin
  LogInsane(Self, 'MapControlThreadedDraw - Enter');
  Cancel := False;
  with Sender as TadsMapDrawingThread do
  begin
    {Canvas.}MoveTo(CenterX, CenterY);
    awayStep := Radius/Sides; // How far to step away from center for each side.
    aroundStep := Coils/Sides; // How far to rotate around center for each side. 0 to 1 based.
    aroundRadians := aroundStep * 2 * PI; // Convert aroundStep to radians.
    rotationRadians := Rotation * 2 * PI; // Convert rotation to radians.
    for i := 1 to Sides do // For every side, step around and away from center.
    begin
      away := i * awayStep; // How far away from center
      around := i * aroundRadians + rotationRadians; // How far around the center.
      x := Trunc(centerX + cos(around) * away);
      y := Trunc(centerY + sin(around) * away);
      {Canvas.}LineTo(x, y);
      Check(Cancel);
      if Cancel then
      begin
        LogInsane(Self, 'MapControlThreadedDraw - Exit');
        Exit;
      end{if};
    end{for};
  end{with};
  LogInsane(Self, 'MapControlThreadedDraw - Leave');
end{procedure};

procedure TadsMapControlTestForm.MapControlUnthreadedDraw;
begin
  Logger.Log(Self, ltDebug, 'MapControlUnthreadedDraw');
  with MapControl do
  begin
    Clear;
    MoveTo(10000, 1000);
    LineTo(15000, 1500);
    MoveTo(15000, 1000);
    LineTo(10000, 1500);
    PolyLine([adsMapPoint(11000, 1100), adsMapPoint(12000, 1100),
      adsMapPoint(12000, 1400), adsMapPoint(11000, 1400)]);
    Polygon([adsMapPoint(14000, 1100), adsMapPoint(13000, 1100),
      adsMapPoint(13000, 1400), adsMapPoint(14000, 1400)]);
    Point(adsMapPoint(11000, 500), 1);
    Point(adsMapPoint(12000, 500), 2);
    Point(adsMapPoint(13000, 500), 3);
    Point(adsMapPoint(14000, 500), 4);
  end{with};
end;

procedure TadsMapControlTestForm.FormCreate(Sender: TObject);
begin
  Logger.AddSubscription(ChangeFileExt(Application.ExeName, '.log'),
    [ltError, ltWarning, ltInformation, ltDebug, ltInsane]);

  MapControl := TadsMapControl.Create(Self);
  with MapControl do
  begin
    Name := 'DrawingArea';
    Parent := Self;
    Left := 0;
    Width := 800;
    Top := 0;
    Height := 400;
    Color := clWhite;
    Anchors := [akLeft, akTop, akRight, akBottom];
    OnBoundsChanged := MapControlBoundsChanged;
    OnSelect := MapControlSelect;
    OnMouseMove := MapControlMouseMove;
    DrawMethod := MapControlThreadedDraw;
    Initialize(10000, 0, 15000, 2500);
    StartDrawing;
  end;
end;

procedure TadsMapControlTestForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    MapControl.Threaded := True;
    MapControl.StartDrawing;
  end
  else
  begin
    MapControl.StopDrawing;
    MapControl.Threaded := False;
    MapControlUnthreadedDraw;
  end{else};
end;

end.
