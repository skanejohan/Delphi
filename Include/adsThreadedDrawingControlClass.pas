unit adsThreadedDrawingControlClass;

{ © 2009 Aston Design Studio }

{**
  TadsThreadedDrawingControl extends TCustomControl with the capacity for
  drawing on the control's canvas in a separate thread. The control is
  updated with regular intervals, and a work-intensive drawing procedure
  works in a separate thread, thus not blocking the UI.

  The control is used in the following manner:
  1. Create a TadsThreadedDrawingControl and place it on a form, frame etc.
  2. Set the DrawMethod property to point to a method responsible for performing
     the actual drawing.
  3. To start drawing, call the StartDrawing method. This will call your drawing
     method to be called in the separate thread.
  4. Implement the drawing method so that it splits up the drawing in more
     manageable chunks (typically a heavy drawing method is implemented by
     traversing a lot of objects and drawing each one of them). Make sure all
     drawing is performed on the canvas of the TadsDrawingThread object supplied
     when the drawing method is called. Also make sure to call the Check method
     of the TadsDrawingThread object regularly, and exit the drawing method if
     the Cancel variable becomes True.
  5. To terminate drawing before everything is drawn, call StopDrawing. Calling
     StartDrawing again while drawing is in progress will cause the StopDrawing
     method to be called implicitly.

  Note: if the drawing method accesses data that may also be accessed from the
  caller thread, it is your responsibility to make sure that this is done in a
  safe manner.
}

interface

uses
  Classes, Controls, Graphics;

type
  TadsDrawingThread = class;
  TadsThreadedDrawingControl = class;

  {** This type is used for your drawing method. }
  TadsDrawMethod = procedure(Sender: TadsDrawingThread) of object;

  {** This is the thread in which the actual drawing takes place. }
  TadsDrawingThread = class(TThread)
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetCanvas: TCanvas;
  protected
    Active: Boolean;
    Activated: Boolean;
    InternalBitmap: TBitmap;
    LastUpdateAt: TDateTime;
    DrawingControl: TadsThreadedDrawingControl;
    procedure Update; {Executed in the main thread}
    procedure Execute; override;
  public

    {** The width of the drawing area. }
    property Width: Integer read GetWidth;

    {** The width of the drawing area. }
    property Height: Integer read GetHeight;

    {** The canvas on which all drawing operations should be performed. }
    property Canvas: TCanvas read GetCanvas;

    {** This method needs to be called by your drawing method (see TadsThreaded-
     DrawingControl.DrawMethod) regularly. It is responsible for the visual
     updates with regular intervals and will set the Cancel parameter to True
     when the thread has terminated, and your drawing method should exit. }
    procedure Check(var Cancel: Boolean);

    constructor Create(aDrawingControl: TadsThreadedDrawingControl); virtual;
    destructor Destroy; override;
  end{class};

  TadsDrawingThreadClass = class of TadsDrawingThread;

  {** Extends TCustomControl with the capacity for drawing on the control's
   canvas in a separate thread. }
  TadsThreadedDrawingControl = class(TCustomControl)
  private
    FThreaded: Boolean;
    FDrawMethod: TadsDrawMethod;
    FUpdateIntervalMS: Integer;
    FDrawingThreadClass: TadsDrawingThreadClass;
  protected
    InternalBitmap: TBitmap;
    DrawingThread: TadsDrawingThread;
    procedure Paint; override;

    {** This method is called when the drawing thread has been created
     (suspended), but before it is resumed. Here you may set any properties
     etc. that your derived thread class needs. }
    procedure DoAfterCreateThread; virtual;
  public

    {** This is the canvas background color. Default is clWhite. }
    property Color;

    {** This property controls whether drawing operations are performed in a
     separate thread (True), with the use of an external drawing method, or if
     drawing is performed in the caller thread, drawing directly on the Canvas
     (False). Default is True. }
    property Threaded: Boolean read FThreaded write FThreaded;

    {** If you use the class in unthreaded mode, you make your drawing operations
     on this canvas. Nothing prevents you from drawing on this canvas also if
     you run in threaded mode, but anything you draw will be overwritten by the
     drawing thread's drawing operations. }
    property Canvas;

    {** Pointer to the method that will be called (in the drawing thread) and
     perform the actual drawing. }
    property DrawMethod: TadsDrawMethod read FDrawMethod write FDrawMethod;

    {** This is the interval, in milliseconds, between successive updates of the
     visible canvas when running in threaded mode. Default is 100. }
    property UpdateIntervalMS: Integer read FUpdateIntervalMS
      write FUpdateIntervalMS;

    {** This is the class of the thread you want to create for the actual
     drawing. By default, a TadsDrawingThread object is used, but you may derive
     from TadsDrawingThread, and set this property to the new class type. That
     way, the drawing control will create a thread object of the desired type
     when drawing should be performed. }
    property DrawingThreadClass: TadsDrawingThreadClass read FDrawingThreadClass
      write FDrawingThreadClass;

    {** If you run in threaded mode, this method will cause your DrawMethod to
     be called in the drawing thread. Before this, any current drawing action
     will be cancelled. If you run in unthreaded mode, nothing happens. }
    procedure StartDrawing;

    {** Call this method to cancel the current drawing action and update the
     visible canvas with what was drawn up until now. }
    procedure StopDrawing;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end{class};

implementation

uses
  SysUtils;

{---------- TadsDrawingThread -------------------------------------------------}

function TadsDrawingThread.GetWidth: Integer;
begin
  Result := InternalBitmap.Width;
end{function};

function TadsDrawingThread.GetHeight: Integer;
begin
  Result := InternalBitmap.Height;
end{function};

function TadsDrawingThread.GetCanvas: TCanvas;
begin
  Result := InternalBitmap.Canvas;
end{function};

procedure TadsDrawingThread.Check(var Cancel: Boolean);
begin
  if not Active then
    Cancel := True
  else if (Now - LastUpdateAt) > (DrawingControl.UpdateIntervalMS / MSecsPerDay) then
  begin
    LastUpdateAt := Now;
    InternalBitmap.Canvas.Unlock;
    Synchronize(Update);
    InternalBitmap.Canvas.Lock;
  end{if};
end{procedure};

procedure TadsDrawingThread.Update;
begin
  if not Terminated then
  begin
    DrawingControl.InternalBitmap.Canvas.Draw(0, 0, InternalBitmap);
    DrawingControl.Invalidate;
  end{if};
end{procedure};

procedure TadsDrawingThread.Execute;
begin
  FreeOnTerminate := False;
  LastUpdateAt := Now;
  while not Terminated do
    if Activated then
    begin
      Activated := False;
      with InternalBitmap do
      begin
        Width := DrawingControl.InternalBitmap.Width;
        Height := DrawingControl.InternalBitmap.Height;
        Canvas.Lock;
        Canvas.Brush.Color := DrawingControl.Color;
        Canvas.FillRect(Rect(0, 0, Width, Height));
      end{with};
      DrawingControl.DrawMethod(Self);
      InternalBitmap.Canvas.Unlock;
      Synchronize(Update);
    end{while};
end{procedure};

constructor TadsDrawingThread.Create(aDrawingControl: TadsThreadedDrawingControl);
begin
  inherited Create(True);
  Active := False;
  Activated := False;
  DrawingControl := aDrawingControl;
  InternalBitmap := TBitmap.Create;
  InternalBitmap.Width := aDrawingControl.InternalBitmap.Width;
  InternalBitmap.Height := aDrawingControl.InternalBitmap.Height;
end{constructor};

destructor TadsDrawingThread.Destroy;
begin
  InternalBitmap.Free;
  InternalBitmap := nil;
  inherited;
end{destructor};

{---------- TadsThreadedDrawingControl ----------------------------------------}

procedure TadsThreadedDrawingControl.Paint;
begin
  inherited;
  if Threaded then
    Canvas.Draw(0, 0, InternalBitmap);
end{procedure};

procedure TadsThreadedDrawingControl.DoAfterCreateThread;
begin
end{procedure};

procedure TadsThreadedDrawingControl.StartDrawing;
var
  ThreadCreated: Boolean;
begin
  if Threaded then
  begin
    if Assigned(DrawingThread) then
    begin
      StopDrawing;
      ThreadCreated := False;
    end{if}
    else
    begin
      DrawingThread := FDrawingThreadClass.Create(Self);
      ThreadCreated := True;
    end{else};
    InternalBitmap.Width := Width;
    InternalBitmap.Height := Height;
    DrawingThread.Active := True;
    DrawingThread.Activated := True;
    if ThreadCreated then
    begin
      DoAfterCreateThread;
      DrawingThread.Resume;
    end{if};
  end{if};
end{procedure};

procedure TadsThreadedDrawingControl.StopDrawing;
begin
  if Assigned(DrawingThread) then
    DrawingThread.Active := False;
end{procedure};

constructor TadsThreadedDrawingControl.Create(AOwner: TComponent);
begin
  inherited;
  Color := clWhite;
  FThreaded := True;
  DrawingThread := nil;
  DoubleBuffered := True;
  UpdateIntervalMS := 100;
  InternalBitmap := TBitmap.Create;
  FDrawingThreadClass := TadsDrawingThread;
end{constructor};

destructor TadsThreadedDrawingControl.Destroy;
begin
  try
    if Assigned(DrawingThread) then
    begin
      StopDrawing;
      DrawingThread.Terminate;
      DrawingThread.WaitFor;
      DrawingThread.Free;
      DrawingThread := nil;
    end{if};
  except
    ; { Suppress a possible "Control has no parent window" }
  end{except};
  InternalBitmap.Free;
  inherited;
end{destructor};

end.
