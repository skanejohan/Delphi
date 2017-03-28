unit adsTextClass;

{
  © 2007-2010 Aston Design Studio

  The TadsText class represents a text that can be written. It is derived from
  the TadsSprite class. The class is created with the font image as parameter,
  thereby creating a regular sprite with the font image as its picture. When the
  SetText method is called, a new image is created which represents the actual
  text, and the sprite will now use that image in all its drawing operations.

  Note that for text objects whose actual text will change (causing the sprite's
  size to change), the AlwaysStore property must be set to True.
}

interface

uses
  SDL,
  adsImageClass,
  adsSpriteClass;

type
  TadsText = class(TadsSprite)
  protected
    FontImage: TadsImage;
  public

    {** Call this method to create a sprite image of the given text. }
    procedure SetText(const Text: String);

    constructor CreateFromFile(aSurface: PSDL_Surface; const FileName: String); override;
    constructor Create(aSurface: PSDL_Surface; aImage: TadsImage; const Description: String); override;
    destructor Destroy; override;
  end; {class}

implementation

uses
  SysUtils;

{---------- TadsText ----------------------------------------------------------}

procedure TadsText.SetText(const Text: String);
var
  ID: TGUID;
  Dest: SDL_Rect;
  i, Frame, w, h, Left: Integer;
  TempSurface, TextSurface: PSDL_Surface;
begin
  if Assigned(FontImage) then
    FImage.Free {In this case Image is the latest text image.}
  else
    FontImage := FImage; {In this case (the first time) Image is the font.}
  {Calculate the text image's size}
  if Text = '' then
    w := 1
  else
  begin
    w := 0;
    for i := 1 to Length(Text) do
    begin
      Frame := Ord(Text[i]);
      w := w + FontImage.IndividualFrameWidth[Frame];
    end; {for}
  end; {else}
  h := FontImage.FrameHeight;
  {Create the text surface with correct settings}
  TempSurface := SDL_CreateRGBSurface(SDL_HWSURFACE, w, h, 32, 0, 0, 0, 0);
  if FontImage.Transparent then
    SDL_SetColorKey(TempSurface, SDL_SRCCOLORKEY, SDL_MapRGB(TempSurface^.format,
      FontImage.Transparent_R, FontImage.Transparent_G, FontImage.Transparent_B));
  TextSurface := SDL_DisplayFormat(TempSurface);
  SDL_FreeSurface(TempSurface);
  {Blit the text to the text surface}
  Left := 0;
  Dest.y := 0;
  Dest.h := h;
  for i := 1 to Length(Text) do
  begin
    Frame := Ord(Text[i]);
    Dest.x := Left;
    Dest.w := FontImage.IndividualFrameWidth[Frame];
    SDL_BlitSurface(FontImage.Surface, FontImage.ImageRect[Frame], TextSurface, @Dest);
    Left := Left + FontImage.IndividualFrameWidth[Frame];
  end; {for}
  {Create a new image with the text surface}
  CreateGUID(ID);
  FImage := TadsImage.Create(Text + GUIDToString(ID), TextSurface,
    FontImage.Transparent, FontImage.Transparent_R, FontImage.Transparent_G,
    FontImage.Transparent_B);
  SDL_FreeSurface(TextSurface);
  if Assigned(FImage.Surface) then
  begin
    SDL_FreeSurface(CoveredBackground);
    CoveredBackground := SDL_DisplayFormat(FImage.Surface);
    SDL_SetColorKey(CoveredBackground, 0, 0);
  end; {if}
  Place(PositionX, PositionY, PositionMode);
end; {procedure}

constructor TadsText.CreateFromFile(aSurface: PSDL_Surface; const FileName: String);
begin
  inherited;
  FontImage := nil;
end; {constructor}

constructor TadsText.Create(aSurface: PSDL_Surface; aImage: TadsImage; const Description: String);
begin
  inherited;
  FontImage := nil;
end; {constructor}

destructor TadsText.Destroy;
begin
  FontImage.Free;
  inherited;
end; {destructor}

end.
