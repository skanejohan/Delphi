unit _adsThreadedDrawingControlTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, adsThreadedDrawingControlClass, StdCtrls, Spin, ExtCtrls;

type
  TadsThreadedDrawingControlTestForm = class(TForm)
    DrawButton: TButton;
    StopButton: TButton;
    UpdateIntervalLabel: TLabel;
    UpdateIntervalSpinEdit: TSpinEdit;
    ApplyButton: TButton;
    StartDrawingTimer: TTimer;
    DrawModeComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure DrawButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure StartDrawingTimerTimer(Sender: TObject);
    procedure DrawModeComboBoxChange(Sender: TObject);
  protected
    TDC: TadsThreadedDrawingControl;
    procedure UnthreadedDraw;
    procedure ThreadedDraw(Sender: TadsDrawingThread);
  end;

var
  adsThreadedDrawingControlTestForm: TadsThreadedDrawingControlTestForm;

implementation

{$R *.dfm}

procedure TadsThreadedDrawingControlTestForm.ThreadedDraw(Sender: TadsDrawingThread);
var
  X, Y: Integer;
  Cancel: Boolean;
begin
  Cancel := False;
  Sender.Canvas.MoveTo(0, 0);
  while True do
    begin
      X := Random(3 * Sender.Width) - Sender.Width;
      Y := Random(3 * Sender.Height) - Sender.Height;
      Sender.Canvas.Pen.Color := Random(16777215);
      Sender.Canvas.LineTo(X, Y);
      Sender.Check(Cancel);
      if Cancel then
        Exit;
    end{for};
end{procedure};

procedure TadsThreadedDrawingControlTestForm.UnthreadedDraw;
var
  i, X, Y: Integer;
begin
  for i := 0 to 1000000 do
    begin
      X := Random(TDC.Width);
      Y := Random(TDC.Height);
      TDC.Canvas.Brush.Color := Random(16777215);
      TDC.Canvas.FillRect(Rect(X, Y, X+1, Y+1));
    end{for};
end{procedure};

procedure TadsThreadedDrawingControlTestForm.DrawButtonClick(Sender: TObject);
begin
  if DrawModeComboBox.ItemIndex in [0, 1] then
    TDC.StartDrawing
  else
    UnthreadedDraw;
end{procedure};

procedure TadsThreadedDrawingControlTestForm.StopButtonClick(Sender: TObject);
begin
  TDC.StopDrawing;
end{procedure};

procedure TadsThreadedDrawingControlTestForm.ApplyButtonClick(Sender: TObject);
begin
  TDC.UpdateIntervalMS := UpdateIntervalSpinEdit.Value;
end{procedure};

procedure TadsThreadedDrawingControlTestForm.StartDrawingTimerTimer(Sender: TObject);
begin
  StartDrawingTimer.Enabled := False;
  if Random(500) = 1 then
    StartDrawingTimer.Interval := 1000
  else
    StartDrawingTimer.Interval := 1;
  if TDC.Threaded then
    TDC.StartDrawing;
  StartDrawingTimer.Enabled := True;
end{procedure};

procedure TadsThreadedDrawingControlTestForm.DrawModeComboBoxChange(Sender: TObject);
begin
  case DrawModeComboBox.ItemIndex of
    0: {threaded}
    begin
      TDC.Threaded := True;
      StartDrawingTimer.Enabled := False;
      StopButton.Enabled := True;
      DrawButtonClick(DrawButton);
    end;
    1: {threaded with timer}
    begin
      TDC.Threaded := True;
      StartDrawingTimer.Enabled := True;
      StopButton.Enabled := False;
      DrawButtonClick(DrawButton);
    end;
    2: {unthreaded}
    begin
      TDC.StopDrawing;
      TDC.Threaded := False;
      StartDrawingTimer.Enabled := False;
      StopButton.Enabled := False;
    end;
  end;
end{procedure};

procedure TadsThreadedDrawingControlTestForm.FormCreate(Sender: TObject);
begin
  TDC := TadsThreadedDrawingControl.Create(Self);
  TDC.Left := 10;
  TDC.Top := 10;
  TDC.Width := 600;
  TDC.Height := 400;
  TDC.Parent := Self;
  TDC.Anchors := [akLeft, akTop, akRight, akBottom];
  TDC.DrawMethod := ThreadedDraw;
  UpdateIntervalSpinEdit.Value := TDC.UpdateIntervalMS;
  DrawModeComboBoxChange(DrawModeComboBox);
end{procedure};

procedure TadsThreadedDrawingControlTestForm.FormResize(Sender: TObject);
begin
  TDC.StartDrawing;
end{procedure};

end.
