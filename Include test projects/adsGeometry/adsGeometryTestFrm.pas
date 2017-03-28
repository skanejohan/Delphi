unit adsGeometryTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, adsGeometry;

type
  TForm1 = class(TForm)
    SameSideImage: TImage;
    SameSideInfoLabel: TLabel;
    SameSideClearButton: TButton;
    SameSideResultLabel: TLabel;
    IntersectInfoLabel: TLabel;
    IntersectImage: TImage;
    IntersectResultLabel: TLabel;
    IntersectClearButton: TButton;
    InsideInfoLabel: TLabel;
    InsideImage: TImage;
    InsideResultLabel: TLabel;
    InsideClearButton: TButton;
    procedure SameSideClearButtonClick(Sender: TObject);
    procedure SameSideImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure IntersectClearButtonClick(Sender: TObject);
    procedure IntersectImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure InsideClearButtonClick(Sender: TObject);
    procedure InsideImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    Mark: Boolean;
    SameSideClickedCount: Integer;
    SameSideLine: TRect;
    SameSideP1, SameSideP2: TPoint;
    IntersectClickedCount: Integer;
    InsidePoint: TPoint;
    InsidePolygon: array of TPoint; 
    IntersectLine1, IntersectLine2: TRect;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.SameSideClearButtonClick(Sender: TObject);
begin
  with SameSideImage.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, 200, 200));
    MoveTo(10, 10);
    LineTo(170, 190);
    SameSideLine.Left := 10;
    SameSideLine.Top := 10;
    SameSideLine.Right := 170;
    SameSideLine.Bottom := 190;
    SameSideClickedCount := 0;
    Brush.Color := clBlack;
    SameSideResultLabel.Caption := 'Result:';
  end;
end;

procedure TForm1.SameSideImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Res: Integer;
begin
  if SameSideClickedCount = 0 then
  begin
    SameSideP1.X := X;
    SameSideP1.Y := Y;
    SameSideClickedCount := 1;
    SameSideImage.Canvas.FillRect(Rect(X-1, Y-1, X+1, Y+1));
  end {if}
  else if SameSideClickedCount = 1 then
  begin
    SameSideClickedCount := 2;
    SameSideImage.Canvas.FillRect(Rect(X-1, Y-1, X+1, Y+1));
    SameSideP2.X := X;
    SameSideP2.Y := Y;
    Res := OnSameSide(SameSideLine, SameSideP1, SameSideP2);
    if Res > 0 then
      SameSideResultLabel.Caption := 'Result: Yes'
    else if Res < 0 then
      SameSideResultLabel.Caption := 'Result: No'
    else
      SameSideResultLabel.Caption := 'Result: One point on the line';
  end; {else}
end;

procedure TForm1.IntersectClearButtonClick(Sender: TObject);
begin
  with IntersectImage.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, 200, 200));
    MoveTo(10, 10);
    LineTo(170, 190);
    IntersectLine1.Left := 10;
    IntersectLine1.Top := 10;
    IntersectLine1.Right := 170;
    IntersectLine1.Bottom := 190;
    IntersectClickedCount := 0;
    Brush.Color := clBlack;
    IntersectResultLabel.Caption := 'Result:';
  end;
end;

procedure TForm1.IntersectImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  IntersectionPoint: TPoint;
begin
  if IntersectClickedCount = 0 then
  begin
    IntersectLine2.Left := X;
    IntersectLine2.Top := Y;
    IntersectClickedCount := 1;
    IntersectImage.Canvas.FillRect(Rect(X-1, Y-1, X+1, Y+1));
  end {if}
  else if IntersectClickedCount = 1 then
  begin
    IntersectClickedCount := 2;
    IntersectImage.Canvas.FillRect(Rect(X-1, Y-1, X+1, Y+1));
    IntersectLine2.Right := X;
    IntersectLine2.Bottom := Y;
    IntersectImage.Canvas.MoveTo(IntersectLine2.Left, IntersectLine2.Top);
    IntersectImage.Canvas.LineTo(IntersectLine2.Right, IntersectLine2.Bottom);
    if Intersect(IntersectLine1, IntersectLine2) then
    begin
      IntersectionPoint := IntersectAt(IntersectLine1, IntersectLine2);
      with IntersectionPoint do
        IntersectImage.Canvas.FillRect(Rect(X-2, Y-2, X+2, Y+2));
      IntersectResultLabel.Caption := 'Result: Yes';
    end{if}
    else
      IntersectResultLabel.Caption := 'Result: No';
  end; {else}
end;

procedure TForm1.InsideClearButtonClick(Sender: TObject);
var
  X, Y: Integer;
begin
  Mark := not Mark;
  with InsideImage.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, 200, 201));
    MoveTo(10, 60);
    LineTo(30, 20);
    LineTo(60, 20);
    LineTo(60, 40);
    LineTo(80, 20);
    LineTo(150, 50);
    LineTo(170, 90);
    LineTo(160, 190);
    LineTo(150, 100);
    LineTo(100, 100);
    LineTo(80, 180);
    LineTo(50, 90);
    LineTo(30, 130);
    LineTo(10, 60);
    SetLength(InsidePolygon, 13);
    InsidePolygon[0].X := 10;
    InsidePolygon[0].Y := 60;
    InsidePolygon[1].X := 30;
    InsidePolygon[1].Y := 20;
    InsidePolygon[2].X := 60;
    InsidePolygon[2].Y := 20;
    InsidePolygon[3].X := 60;
    InsidePolygon[3].Y := 40;
    InsidePolygon[4].X := 80;
    InsidePolygon[4].Y := 20;
    InsidePolygon[5].X := 150;
    InsidePolygon[5].Y := 50;
    InsidePolygon[6].X := 170;
    InsidePolygon[6].Y := 90;
    InsidePolygon[7].X := 160;
    InsidePolygon[7].Y := 190;
    InsidePolygon[8].X := 150;
    InsidePolygon[8].Y := 100;
    InsidePolygon[9].X := 100;
    InsidePolygon[9].Y := 100;
    InsidePolygon[10].X := 80;
    InsidePolygon[10].Y := 180;
    InsidePolygon[11].X := 50;
    InsidePolygon[11].Y := 90;
    InsidePolygon[12].X := 30;
    InsidePolygon[12].Y := 130;
    if Mark then
    begin
      for X := 0 to InsideImage.Picture.Width-1 do
        for y := 0 to InsideImage.Picture.Height-1 do
        begin
          InsidePoint.X := X;
          InsidePoint.Y := Y;
          if Inside(InsidePoint, InsidePolygon) then
            InsideImage.Canvas.Brush.Color := clGreen
          else
            InsideImage.Canvas.Brush.Color := clRed;
          InsideImage.Canvas.FillRect(Rect(X-1, Y-1, X+1, Y+1));
        end; {for}
    end; {if}
    Brush.Color := clBlack;
    IntersectResultLabel.Caption := '';
  end;
end;

procedure TForm1.InsideImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  InsidePoint.X := X;
  InsidePoint.Y := Y;
  if Inside(InsidePoint, InsidePolygon) then
    InsideResultLabel.Caption := Format('(%d,%d): Yes', [X, Y])
  else
    InsideResultLabel.Caption := Format('(%d,%d): No', [X, Y])
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SameSideClearButtonClick(nil);
  IntersectClearButtonClick(nil);
  Mark := True; {Will be set to False in InsideClearButtonClick}
  InsideClearButtonClick(nil);
end; {procedure}

end.
