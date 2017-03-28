unit adsImageClass;

{
  © 2007-2010 Aston Design Studio

  The TadsImage class represents an image which can be drawn on a surface.
  Internally, the class uses a global list of images ensuring that each image
  file is read into memory only once.

}

interface

uses
  SDL,
  adsObjectClass;

type
  TadsImage = class(TadsObject)
  private
    FFrameWidth: Integer;
    FFrameHeight: Integer;
    FFrameCount: Integer;
    FImageRect: SDL_Rect;
    FTransparent: Boolean;
    FTransparent_R: Byte;
    FTransparent_G: Byte;
    FTransparent_B: Byte;
    FSurface: PSDL_Surface;
    FIndividualFrameWidths: Array of Integer;
    procedure SetFrameHeight(const Value: Integer);
    procedure SetFrameWidth(const Value: Integer);
    function GetImageRect(Frame: Integer): PSDL_Rect;
    function GetIndividualFrameWidth(Index: Integer): Integer;
    function GetPixel(X, Y: Integer): Cardinal;
    procedure SetPixel(X, Y: Integer; const Value: Cardinal);
  protected
    ImageFileName: String;
    VerticalFrameCount: Integer;
    HorizontalFrameCount: Integer;
    procedure CalculateFrameCount;
    function LockSurface: Boolean;
    procedure UnlockSurface;
  public

    {** Access to this image's underlying surface.}
    property Surface: PSDL_Surface read FSurface;

    {** Indicates if this image is transparent. }
    property Transparent: Boolean read FTransparent;

    {** If this image is transparent, this is the "R" (as in "RGB") component of
     the "transparent" color. }
    property Transparent_R: Byte read FTransparent_R;

    {** If this image is transparent, this is the "G" (as in "RGB") component of
     the "transparent" color. }
    property Transparent_G: Byte read FTransparent_G;

    {** If this image is transparent, this is the "B" (as in "RGB") component of
     the "transparent" color. }
    property Transparent_B: Byte read FTransparent_B;

    {** The number of frames in this image. This is calculated automatically
     when FrameWidth or FrameHeight is set. }
    property FrameCount: Integer read FFrameCount;

    {** The width of each frame in this image. When this property is set, it
     automatically recalculates FrameCount. }
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;

    {** The height of each frame in this image. When this property is set, it
     automatically recalculates FrameCount. }
    property FrameHeight: Integer read FFrameHeight write SetFrameHeight;

    {** A pointer to the rectangle in the image that represents the given frame,
     for use e.g. as source rectangle in a blitting operation. }
    property ImageRect[Frame: Integer]: PSDL_Rect read GetImageRect;

    {** This is the width of an individual frame, which is used for e.g. fonts,
     where all characters don't have the same width. }
    property IndividualFrameWidth[Index: Integer]: Integer
      read GetIndividualFrameWidth;

    {** Get or set the color of the pixel at specified coordinates. }
    property Pixel[X, Y: Integer]: Cardinal read GetPixel write SetPixel;

    {** Creates and returns a copy of this object. }
    function Clone: TadsImage;

    {** When an image is constructed, it will be loaded from the given file. In
     addition to the graphic file, there may be an inifile with the same name,
     but extension '.ini' which holds the following information:

     [image]
     framewidth=xxx
     frameheight=xxx
     transparent=0 | 1
     transparent_r = 0..255
     transparent_g = 0..255
     transparent_b = 0..255

     [widths]
     65=...
     66=...

     If the ini file, or part of it, is missing the frame width will be set to
     equal the width of the image (giving a frame count of 1), transparent will
     be set to False and the R, G and B components of the transparent color will
     all be set to 0. The optional "widths" section defines widths for
     particular frames, which is typically used for an image representing a
     font. For all frames that are not represented here, the width is set to
     the value of the FrameWidth property. }
    constructor CreateFromFile(const FileName: String);

    {** Constructs a new image, using an existing surface as template. This
     constructor is used by the TadsText class}
    constructor Create(const Identifier: String; const aSurface: PSDL_Surface;
      Transparent: Boolean; Transparent_R, Transparent_G, Transparent_B: Byte);

    destructor Destroy; override;
  end; {class}
  
implementation

uses
  Classes,
  IniFiles,
  SysUtils,
  SDL_Image,
  adsLoggerClass;

{---------- TadsImageSurfaces -------------------------------------------------}
{---------- Used by TadsImage, but aren't accessible outside this unit --------}

type
  TadsImageSurface = class(TadsObject)
    Surface: PSDL_Surface;
    Counter: Integer;
    T: Boolean;
    T_R: Byte;
    T_G: Byte;
    T_B: Byte;
    constructor Create(const Identifier: String);
  end; {class}

  TadsImageSurfaces = class(TadsObject)
  protected
    ImageSurfaceList: TStringList;
    function Use(const Identifier: String; out Index: Integer): TadsImageSurface;
    function New(const Identifier: String; aSurface: PSDL_Surface; T: Boolean;
      T_R, T_G, T_B: Byte): TadsImageSurface;
    procedure Return(const Identifier: String);
  public
    function Add(const aIdentifier: String; aSurface: PSDL_Surface; T: Boolean;
      T_R, T_G, T_B: Byte): TadsImageSurface;
    function AddFromFile(const FileName: String): TadsImageSurface;
    procedure Remove(const Identifier: String);
    constructor Create;
    destructor Destroy; override;
  end; {class}

constructor TadsImageSurface.Create(const Identifier: String);
begin
  Description := Identifier;
end; {constructor}

function TadsImageSurfaces.Use(const Identifier: String; out Index: Integer):
  TadsImageSurface;
begin
  if ImageSurfaceList.Find(Identifier, Index) then
  begin
    Result := TadsImageSurface(ImageSurfaceList.Objects[Index]);
    with Result do
    begin
      Inc(Counter);
      LogDebug(Self, Format('Image surface count increased to %d for "%s"',
        [Counter, ExtractFileName(Identifier)]));
    end; {with}
  end {if}
  else
    Result := nil;
end; {function}

function TadsImageSurfaces.New(const Identifier: String; aSurface: PSDL_Surface;
  T: Boolean; T_R, T_G, T_B: Byte): TadsImageSurface;
begin
  Result := TadsImageSurface.Create(Identifier);
  Result.Counter := 1;
  Result.Surface := aSurface;
  Result.T := T;
  Result.T_R := T_R;
  Result.T_G := T_G;
  Result.T_B := T_B;
  ImageSurfaceList.AddObject(Identifier, Result);
  LogDebug(Self, Format('Image surface created for "%s"',
    [ExtractFileName(Identifier)]));
end; {function}

procedure TadsImageSurfaces.Return(const Identifier: String);
var
  Index: Integer;
  AIS: TadsImageSurface;
begin
  if ImageSurfaceList.Find(Identifier, Index) then
  begin
    AIS := TadsImageSurface(ImageSurfaceList.Objects[Index]);
    Dec(AIS.Counter);
    LogDebug(Self, Format('Image surface count decreased to %d for "%s"',
      [AIS.Counter, ExtractFileName(Identifier)]));
    if AIS.Counter = 0 then
    begin
      SDL_FreeSurface(AIS.Surface);
      AIS.Free;
      ImageSurfaceList.Delete(Index);
      LogDebug(Self, Format('Image surface removed for "%s"',
        [ExtractFileName(Identifier)]));
    end; {if}
  end; {if}
end; {function}

function TadsImageSurfaces.Add(const aIdentifier: String; aSurface: PSDL_Surface;
  T: Boolean; T_R, T_G, T_B: Byte): TadsImageSurface;
var
  Index: Integer;
begin
  Result := Use(aIdentifier, Index);
  if not Assigned(Result) then
    Result := New(aIdentifier, SDL_DisplayFormat(aSurface), T, T_R, T_G, T_B);
end; {function}

function TadsImageSurfaces.AddFromFile(const FileName: String): TadsImageSurface;
var
  T: Boolean;
  Index: Integer;
  IniFile: TIniFile;
  T_R, T_G, T_B: Byte;
  TempSurface: PSDL_Surface;
begin
  Result := Use(FileName, Index);
  if not Assigned(Result) then
  begin
    TempSurface := IMG_Load(PChar(FileName));
    if Assigned(TempSurface) then
    begin
      IniFile := TIniFile.Create(ChangeFileExt(FileName, '.ini'));
      try
        T := IniFile.ReadBool('image', 'transparent', False);
        T_R := IniFile.ReadInteger('image', 'transparent_r', 0);
        T_G := IniFile.ReadInteger('image', 'transparent_g', 0);
        T_B := IniFile.ReadInteger('image', 'transparent_b', 0);
        if T then
          SDL_SetColorKey(TempSurface, SDL_SRCCOLORKEY, SDL_MapRGB(
            TempSurface^.format, T_R, T_G, T_B));
        Result := New(FileName, SDL_DisplayFormat(TempSurface), T, T_R, T_G, T_B);
        SDL_FreeSurface(TempSurface);
      finally
        IniFile.Free;
      end; {finally}
    end {if}
    else
    begin
      LogError(Self, Format('IMG_Load(%s) failed', [FileName]));
      Result := nil;
    end; {else}
  end; {else}
end; {procedure}

procedure TadsImageSurfaces.Remove(const Identifier: String);
begin
  Return(Identifier);
end; {procedure}

constructor TadsImageSurfaces.Create;
begin
  Description := '';
  ImageSurfaceList := TStringList.Create;
  ImageSurfaceList.Sorted := True;
  ImageSurfaceList.CaseSensitive := False;
end; {constructor}

destructor TadsImageSurfaces.Destroy;
var
  i: Integer;
  AIS: TadsImageSurface;
begin
  for i := 0 to ImageSurfaceList.Count-1 do
  begin
    AIS := TadsImageSurface(ImageSurfaceList.Objects[i]);
    SDL_FreeSurface(AIS.Surface);
    AIS.Free;
  end; {for}
  ImageSurfaceList.Free;
  inherited;
end; {destructor}

var
  ImageSurfaces: TadsImageSurfaces;

{---------- TadsImage ---------------------------------------------------------}

procedure TadsImage.SetFrameWidth(const Value: Integer);
begin
  FFrameWidth := Value;
  if FFrameWidth = 0 then
    HorizontalFrameCount := 0
  else
    HorizontalFrameCount := FSurface^.w div FFrameWidth;
  CalculateFrameCount;
end; {procedure}

procedure TadsImage.SetFrameHeight(const Value: Integer);
begin
  FFrameHeight := Value;
  if FFrameHeight = 0 then
    VerticalFrameCount := 0
  else
    VerticalFrameCount := FSurface^.h div FFrameHeight;
  CalculateFrameCount;
end; {function}

function TadsImage.GetImageRect(Frame: Integer): PSDL_Rect;
begin
  FImageRect.x := ((Frame mod HorizontalFrameCount) * FrameWidth) +
    ((FrameWidth - IndividualFrameWidth[Frame]) div 2);
  FImageRect.y := (Frame div HorizontalFrameCount) * FrameHeight;
  FImageRect.w := IndividualFrameWidth[Frame];
  FImageRect.h := FrameHeight;
  Result := @FImageRect;
end; {function}

function TadsImage.GetIndividualFrameWidth(Index: Integer): Integer;
begin
  Result := FIndividualFrameWidths[Index];
end; {function}

type
  TPixelFormat = Array[0..2] of UInt8;
  TPixelArray = Array[0..64000] of UInt8;

function TadsImage.GetPixel(X, Y: Integer): Cardinal;
var
  Bits: Pointer;
  BPP, R, G, B: UInt8;
begin
  Result := 0;
  if LockSurface then
  begin
    try
      BPP := FSurface.format.BytesPerPixel;
      Bits := @(TPixelArray(FSurface.Pixels^)[(Y * FSurface.pitch) + (X * BPP)]);
      case BPP of
        1, 2, 4:
        begin
          Result := PUint32(Bits)^;
        end; {1 = 8 bytes per pixel, 2 = 15 or 16 bpp, 4 = 32 bpp}
        3:
        begin
          R := TPixelFormat(Bits^)[(FSurface.format.Rshift div 8)];
          G := TPixelFormat(Bits^)[(FSurface.format.Gshift div 8)];
          B := TPixelFormat(Bits^)[(FSurface.format.Bshift div 8)];
          Result := SDL_MapRGB(FSurface.format, R, G, B);
        end; { 3 = format/endian independent}
      else
        LogWarning(Self, Format('Unknown BPP (%d) in GetPixel', [BPP]));
      end; {case}
    finally
      UnlockSurface;
    end; {finally}
  end {if}
  else
    LogError(Self, 'Unable to lock surface in GetPixel');
end; {function}

procedure TadsImage.SetPixel(X, Y: Integer; const Value: Cardinal);
var
  Bits: Pointer;
  BPP, R, G, B: UInt8;
begin
  if LockSurface then
  begin
    try
      BPP := FSurface.format.BytesPerPixel;
      Bits := @(TPixelArray(FSurface.Pixels^)[(Y * FSurface.pitch) + (X * bpp)]);
      case BPP of
        1:
        begin
          PUint8(Bits)^ := Uint8(Value);
        end; {1 = 8 bytes per pixel}
        2:
        begin
          PUint16(Bits)^ := Uint16(Value);
        end; {2 = 15 or 16 bytes per pixel}
        3:
        begin
          R := (Value shr FSurface.format.Rshift ) and $FF;
          G := (Value shr FSurface.format.Gshift ) and $FF;
          B := (Value shr FSurface.format.Bshift ) and $FF;
          TPixelFormat(Bits^)[(FSurface.format.Rshift div 8)] := R;
          TPixelFormat(Bits^)[(FSurface.format.Gshift div 8)] := G;
          TPixelFormat(Bits^)[(FSurface.format.Bshift div 8)] := B;
        end; { 3 = format/endian independent }
        4:
        begin
          PUInt32(bits)^ := Uint32(Value);
        end; {4 = 32 bytes per pixel}
      else
        LogWarning(Self, Format('Unknown BPP (%d) in SetPixel', [BPP]));
      end; {case}
    finally
      UnlockSurface;
      SDL_UpdateRect(FSurface, X, Y, 1, 1);
    end; {finally}
  end {if}
  else
    LogError(Self, 'Unable to lock surface in SetPixel');
end; {procedure}

procedure TadsImage.CalculateFrameCount;
begin
  FFrameCount := HorizontalFrameCount * VerticalFrameCount;
end; {procedure}

function TadsImage.LockSurface: Boolean;
begin
  if SDL_MustLock(FSurface) then
    Result := SDL_LockSurface(FSurface) = 0
  else
    Result := True;
end; {function}

procedure TadsImage.UnlockSurface;
begin
  SDL_UnlockSurface(FSurface);
end; {procedure}

function TadsImage.Clone: TadsImage;
begin
  Result := TadsImage.Create(Description, FSurface, FTransparent,
    FTransparent_R, FTransparent_G, FTransparent_B);
end; {function}

constructor TadsImage.CreateFromFile(const FileName: String);
var
  i: Integer;
  IniFile:TIniFile;
  Width, Height: Integer;
  ImageSurface: TadsImageSurface;
begin
  Description := FileName;
  ImageFileName := FileName;
  ImageSurface := ImageSurfaces.AddFromFile(ImageFileName);
  if Assigned(ImageSurface) then
  begin
    FSurface := ImageSurface.Surface;
    IniFile := TIniFile.Create(ChangeFileExt(FileName, '.ini'));
    try
      Width := IniFile.ReadInteger('image', 'framewidth', 0);
      Height := IniFile.ReadInteger('image', 'frameheight', 0);
      if Width = 0 then
        FrameWidth := FSurface^.w
      else
        FrameWidth := Width;
      if Height = 0 then
        FrameHeight := FSurface^.h
      else
        FrameHeight := Height;
      SetLength(FIndividualFrameWidths, FrameCount);
      for i := 0 to FrameCount-1 do
        FIndividualFrameWidths[i] := IniFile.ReadInteger('widths',
          IntToStr(i), FrameWidth);
      FTransparent := ImageSurface.T;
      FTransparent_R := ImageSurface.T_R;
      FTransparent_G := ImageSurface.T_G;
      FTransparent_B := ImageSurface.T_B;
    finally
      IniFile.Free;
    end; {finally}
  end {if}
  else
    LogError(Self, Format('Failed to create image from file %s', [FileName]));
end; {constructor}

constructor TadsImage.Create(const Identifier: String; const aSurface: PSDL_Surface;
  Transparent: Boolean; Transparent_R, Transparent_G, Transparent_B: Byte);
begin
  Description := Identifier;
  ImageFileName := '';
  FSurface := ImageSurfaces.Add(Identifier, aSurface, Transparent,
    Transparent_R, Transparent_G, Transparent_B).Surface;
  FrameWidth := FSurface^.w;
  FrameHeight := FSurface^.h;
  SetLength(FIndividualFrameWidths, 1);
  FIndividualFrameWidths[0] := FrameWidth;
  FTransparent := Transparent;
  FTransparent_R := Transparent_R;
  FTransparent_G := Transparent_G;
  FTransparent_B := Transparent_B;
end; {constructor}

destructor TadsImage.Destroy;
begin
  ImageSurfaces.Remove(ImageFileName);
  inherited;
end; {destructor}

initialization
  ImageSurfaces := TadsImageSurfaces.Create;

finalization
  ImageSurfaces.Free;

end.
