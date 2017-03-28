unit adsImagePartSelectorClass;

{ © 2002-2010 Aston Design Studio }

interface

uses
  Classes, SysUtils, Windows, Graphics, ExtCtrls, Jpeg;

type
  WordPtr=^Word;
  BytePtr=^Byte;

  TLoadBitmapFunction = function(FileName: PChar; NImage: Integer;
    Callback: WordPtr): HBitmap;

  {** The exception type used if an image file was not loaded properly. }
  EFileReadError = class(Exception);

  {** Type used for the Direction parameter of the Pan method. }
  TPanningDirection = (pdUp, pdRight, pdDown, pdLeft);

  {** This class is used for automatic zooming and panning of images. }
  TImagePartSelector = class
  protected
    CurrentSelection: TRect;
    ImageBitmap, PartOfImageBitmap: TBitmap;
    FImage: TImage;
    FClearColor: TColor;
    function CalculateDrawRect(ABitmap: TBitmap): TRect;
    procedure SelectAll;
    procedure SelectPart(ARect: TRect);
    procedure DrawPart;
  public
    constructor Create;
    destructor Destroy; override;

    {** This property is the "background color" of the image component, i.e.
        the color that will be drawn in the image if the selected part of the
        image does not cover the whole image component. Default = clBtnFace. }
    property ClearColor: TColor read FClearColor write FClearColor;

    {** This is the image component whose behaviour should be controlled by this
        object. }
    property Image: TImage read FImage write FImage;

    {** Draw the image. If SelectAll is True, the image drawn will be selected
        to fit all of the TImage component, but its height/width ratio will not
        be changed; hence the image will fill either the full width or the full
        height of the TImage component. If SelectAll is False, the image drawn
        will not be scaled, but it will be centered in the middle of the TImage
        component. }
    procedure Draw(SelectAll: Boolean);

    {** Zoom in on the selection, i.e. select a smaller part of the original
        image. ZoomLevelInPercent indicates how many percent smaller the height
        and width of the selected area should become. Zooming will keep the
        center of the selected part. }
    procedure ZoomIn(ZoomLevelInPercent: Integer);

    {** Zoom out on the selection, i.e. select a larger part of the original
        image. ZoomLevelInPercent indicates how many percent larger the height
        and width of the selected area should become. Zooming will keep the
        center of the selected part. }
    procedure ZoomOut(ZoomLevelInPercent: Integer);

    {** Pan the image in the direction given by Direction. PanLevelInPercent
        indicates how far we should pan, in percent of the image's total width
        or height. }
    procedure Pan(Direction: TPanningDirection; PanLevelInPercent: Integer);

    {** Load an image. If Image is set to point to a TImage object, and a JPeg
        image is loaded, the part selector will use the image's scale factor to
        make sure that the image is loaded in a format making it small enough
        (if possible) to fit inside Image. Note that the image will still not
        be displayed automatically; use the Draw method to show the image. }
    procedure Load(AFileName: TFileName);
  end{class};

implementation

uses
  Math;

constructor TImagePartSelector.Create;
begin
  inherited;
  ImageBitmap := TBitmap.Create;
  PartOfImageBitmap := TBitmap.Create;
  ClearColor := clBtnFace;
end{constructor};

destructor TImagePartSelector.Destroy;
begin
  ImageBitmap.Free;
  PartOfImageBitmap.Free;
  inherited;
end{destructor};

function TImagePartSelector.CalculateDrawRect(ABitmap: TBitmap): TRect;
var
  ImageHWRatio, BitmapHWRatio: Double;
  UsedWidth, UsedHeight: Double;
  Left, Right, Top, Bottom: Integer;
begin
  ImageHWRatio := FImage.Height/FImage.Width;
  try
    BitmapHWRatio := PartOfImageBitmap.Height/PartOfImageBitmap.Width;
  except
    BitmapHWRatio := 1.0; {Image probably not loaded; width = 0}
  end{try/except};
  if BitmapHWRatio > ImageHWRatio then
  begin
    UsedWidth := FImage.Height / BitmapHWRatio;
    UsedHeight := FImage.Height;
  end{if}
  else
  begin
    UsedWidth := FImage.Width;
    UsedHeight := FImage.Width * BitmapHWRatio;
  end{else};
  Left := Trunc((FImage.Width-UsedWidth)/2);
  Right := FImage.Width-Left;
  Top := Trunc((FImage.Height-UsedHeight)/2);
  Bottom := FImage.Height - Top;
  Result := Rect(Left, Top, Right, Bottom);
end{function};

procedure TImagePartSelector.Draw(SelectAll: Boolean);
var
  X, Y: Integer;
begin
  FImage.Picture.Bitmap.Height := FImage.Height;
  FImage.Picture.Bitmap.Width := FImage.Width;
  X := Trunc((FImage.Width-ImageBitmap.Width)/2);
  Y := Trunc((FImage.Height-ImageBitmap.Height)/2);
  with FImage.Canvas do
  begin
    Brush.Color := ClearColor;
    FillRect(ClipRect);
    if SelectAll then
      StretchDraw(CalculateDrawRect(FImage.Picture.Bitmap), PartOfImageBitmap)
    else
      Draw(X, Y, ImageBitmap);
  end{with};
end{procedure};

procedure TImagePartSelector.DrawPart;
begin
  FImage.Picture.Bitmap.Height := FImage.Height;
  FImage.Picture.Bitmap.Width := FImage.Width;
  with FImage.Canvas do
  begin
    Brush.Color := ClearColor;
    FillRect(ClipRect);
    StretchDraw(CalculateDrawRect(FImage.Picture.Bitmap), PartOfImageBitmap);
  end{with};
end{procedure};

procedure TImagePartSelector.SelectAll;
begin
  SelectPart(Rect(0, 0, ImageBitmap.Width, ImageBitmap.Height));
end{procedure};

procedure TImagePartSelector.SelectPart(ARect: TRect);
begin
  CurrentSelection := ARect;
  with PartOfImageBitmap do
  begin
    Width := ARect.Right - ARect.Left;
    Height := ARect.Bottom - ARect.Top;
    Canvas.Brush.Color := ClearColor;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.CopyRect(Canvas.ClipRect, ImageBitmap.Canvas, CurrentSelection);
  end{with};
end{procedure};

procedure TImagePartSelector.ZoomIn(ZoomLevelInPercent: Integer);
var
  HeightMargin, WidthMargin: Integer;
begin
  with CurrentSelection do
  if (Right - Left > ImageBitmap.Width div 10) and
    (Bottom - Top > ImageBitmap.Height div 10) then
  begin
    HeightMargin := Trunc((Bottom - Top) * (ZoomLevelInPercent / 100));
    WidthMargin := Trunc((Right - Left) * (ZoomLevelInPercent / 100));
    SelectPart(Rect(Left + WidthMargin div 2, Top + HeightMargin div 2,
      Right - WidthMargin div 2, Bottom - HeightMargin div 2));
    DrawPart;
  end{with};
end{procedure};

procedure TImagePartSelector.ZoomOut(ZoomLevelInPercent: Integer);
var
  HeightMargin, WidthMargin: Integer;
begin
  with CurrentSelection do
  if (Right - Left < ImageBitmap.Width * 3) and
    (Bottom - Top < ImageBitmap.Height * 3) then
  begin
    HeightMargin := Trunc((Bottom - Top) * (ZoomLevelInPercent / 100));
    WidthMargin := Trunc((Right - Left) * (ZoomLevelInPercent / 100));
    SelectPart(Rect(Left - WidthMargin div 2, Top - HeightMargin div 2,
      Right + WidthMargin div 2, Bottom + HeightMargin div 2));
    DrawPart;
  end{with};
end{procedure};

procedure TImagePartSelector.Pan(Direction: TPanningDirection;
  PanLevelInPercent: Integer);
var
  HeightMargin, WidthMargin: Integer;
begin
  with CurrentSelection do
  begin
    HeightMargin := Trunc((Bottom - Top) * (PanLevelInPercent / 100));
    WidthMargin := Trunc((Right - Left) * (PanLevelInPercent / 100));
    case Direction of
      pdUp:
        SelectPart(Rect(Left, Top-HeightMargin, Right, Bottom-HeightMargin));
      pdRight:
        SelectPart(Rect(Left+WidthMargin, Top, Right+WidthMargin, Bottom));
      pdDown:
        SelectPart(Rect(Left, Top+HeightMargin, Right, Bottom+HeightMargin));
      pdLeft:
        SelectPart(Rect(Left-WidthMargin, Top, Right-WidthMargin, Bottom));
    end{case};
  end{with};
  DrawPart;
end{procedure};

procedure TImagePartSelector.Load(AFileName: TFileName);
var
  Extension: String;
  LoadBitmap: TBitmap;
  Img: TImage;
  JPI: TJPEGImage;
  Scale: TJPEGScale;

  procedure InitialiseImageBitmap(LoadBitmap: TBitmap);
  var
    Offset: Integer;
    SourceRect, DestRect: TRect;
    ImageHWRatio, LoadBitmapHWRatio: Double;
  begin
    ImageHWRatio := FImage.Height / FImage.Width;
    LoadBitmapHWRatio := LoadBitmap.Height / LoadBitmap.Width;
    if ImageHWRatio < LoadBitmapHWRatio then
    begin
      ImageBitmap.Height := LoadBitmap.Height;
      ImageBitmap.Width := Trunc(ImageBitmap.Height / ImageHWRatio);
      Offset := (ImageBitmap.Width - LoadBitmap.Width) div 2;
      DestRect := Rect(Offset, 0, Offset + LoadBitmap.Width, LoadBitmap.Height);
    end{if}
    else
    begin
      ImageBitmap.Width := LoadBitmap.Width;
      ImageBitmap.Height := Trunc(ImageBitmap.Width * ImageHWRatio);
      Offset := (ImageBitmap.Height - LoadBitmap.Height) div 2;
      DestRect := Rect(0, Offset, LoadBitmap.Width, Offset + LoadBitmap.Height);
    end{else};
    SourceRect := Rect(0, 0, LoadBitmap.Width, LoadBitmap.Height);
    ImageBitmap.Canvas.Brush.Color := ClearColor;
    ImageBitmap.Canvas.FillRect(ImageBitmap.Canvas.ClipRect);
    ImageBitmap.Canvas.CopyRect(DestRect, LoadBitmap.Canvas, SourceRect);
  end{procedure};

begin
  Extension := AnsiLowerCase(ExtractFileExt(AFileName));
  JPI := TJPEGImage.Create;
  Img := TImage.Create(nil);
  LoadBitmap := TBitmap.Create;
  try
    if (Extension = '.jpg') or (Extension = '.jpeg') then
      try
        JPI.Scale := jsFullSize;
        JPI.LoadFromFile(AFileName);
        if FImage <> nil then
        begin
          for Scale := jsHalf to jsEighth do
            if (JPI.Height > FImage.Height) or (JPI.Width > FImage.Width) then
              JPI.Scale := Scale;
        end{if};
        LoadBitmap.Assign(JPI);
        InitialiseImageBitmap(LoadBitmap);
        SelectAll;
      except
        Raise EFileReadError.Create('Not a valid JPEG image!');
      end{try/except}
    else if Extension = '.bmp' then
      try
        LoadBitmap.LoadFromFile(AFileName);
        InitialiseImageBitmap(LoadBitmap);
        SelectAll;
      except
        Raise EFileReadError.Create('Not a valid bitmap image!');
      end{try/except}
    else
      Raise EFileReadError.Create('Not a valid image!');
  finally
    JPI.Free;
    Img.Free;
    LoadBitmap.Free;
  end{try/finally};
end{procedure};

end.

