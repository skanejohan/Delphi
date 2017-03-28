unit adsSpriteClass;

{
  © 2007-2010 Aston Design Studio

  The TadsSprite class represents a sprite, and automatically handles drawing,
  animation etc.

}

interface

uses
  Classes,
  Contnrs,
  SDL,
  adsObjectClass,
  adsImageClass;

type
  TadsPositionMode = (pmTopLeft, pmTop, pmTopRight, pmRight, pmBottomRight,
    pmBottom, pmBottomLeft, pmLeft, pmCenter);
  TadsAnimationDirection = (adForward, adBackward);
  TadsAnimationMode = (amForward, amBackward, amForwardAndBackward);

  TadsSpriteState = class(TadsObject)
    ID: Integer;
    FirstIndex: Integer;
    LastIndex: Integer;
    Delay: Integer;

    {** Indicates whether the sprite should animate from the first frame to the
     last and then start over from the first one again (amForward), from the
     last one down to the first one and then start over again (amBackward) or if
     it should animate from the first frame to the last, then backwards to the
     first frame again before starting over (amForwardAndBackward). Default is
     amForward. }
    AnimationMode: TadsAnimationMode;

    constructor Create(const aDescription: String; aID: Integer);
  end; {class}

  TadsSprite = class(TadsObject)
  private
    FStateID: Integer;
    FAlwaysStore: Boolean;
    FPosition: TPoint;
    FOnDestroy: TNotifyEvent;
    FIndexedStateTransition: Boolean;
    FAlpha: Byte;
    procedure SetStateID(const Value: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetAlpha(const Value: Byte);
  protected
    Ticks: Integer;
    Frame: Integer;
    FImage: TadsImage;
    Surface, CoveredBackground: PSDL_Surface;
    StateIndex: Integer;
    StateList: TObjectList; {Of TadsSpriteState}
    Description: String;
    PreviousX: Integer;
    PreviousY: Integer;
    PositionX: Integer;
    PositionY: Integer;
    PositionMode: TadsPositionMode;
    AnimationDirection: TadsAnimationDirection;
    function IndexOfState(ID: Integer): Integer;
    function CurrentState: TadsSpriteState;
    procedure TransferSettings(Dest: TadsSprite); virtual;
    procedure AddState(ID, FirstIndex, LastIndex, Delay: Integer;
      AnimationMode: TadsAnimationMode);
  public

    {** The sprite's width. }
    property Width: Integer read GetWidth;

    {** The sprite's height. }
    property Height: Integer read GetHeight;

    {** The screen position of the upper left corner of the sprite.}
    property Position: TPoint read FPosition;

    {** The underlying image object.}
    property Image: TadsImage read FImage;

    {** The alpha value for the sprite. }
    property Alpha: Byte read FAlpha write SetAlpha;

    {** Read this property for information about the ID of the active state. Set
     this property to make the state with the given ID become the active state. }
    property StateID: Integer read FStateID write SetStateID;

    {** Indicates if we should always store the background behind the sprite
     before drawing the sprite (True) or if we should do it only if the sprite
     hasn't moved since the last time (False). The default value for this
     property is True, which gives slightly better performance, but if you have
     a background that may have changed under the sprite since the last draw, or
     if you have a sprite whose size has changed (typically the case when using
     the SetText method of TadsText), you need to set this to True to make sure
     the background is later restored correctly. }
    property AlwaysStore: Boolean read FAlwaysStore write FAlwaysStore;

    {** This property indicates how the sprite should react when its state
     changes. If IndexedStateTransition is False, the next frame drawn will be
     the first frame of the new state, but if it is True the next frame written
     will be the same number of frames into the new state. A typical example of
     when you want to set this property to True is when you have an animated
     sprite that may or may not be highlighted when the mouse hovers over it.
     In that case, you probably want the animation to continue from where it
     was, but with the "highlight effect" on. Default is False. }
    property IndexedStateTransition: Boolean read FIndexedStateTransition
      write FIndexedStateTransition;

    {** This event strikes immediately before the sprite is destroyed, and it is
     handled by the game state object to remove the sprite from any list where
     it may be included. }
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;

    {** Restores the background, by redawing it in the position where the sprite
     is located. }
    procedure RestoreBackground; virtual;

    {** Animates the sprite. }
    procedure Animate; virtual;

    {** Updates the sprite. Default behaviour is to do nothing, but for an
     inherited sprite class, the sprite could e.g. be moved here. }
    procedure Update; virtual;

    {** Stores the background where the sprite will be drawn.}
    procedure StoreBackground; virtual;

    {** Draw the sprite onto the game's surface. }
    procedure Draw; virtual;

    {** Move the sprite (change Position) by this many pixels in X and Y. }
    procedure Move(X, Y: Integer);

    {** Place the sprite (change Position) at the given coordinates. }
    procedure Place(X, Y: Integer; Mode: TadsPositionMode); overload;
    procedure Place(Coordinates: TPoint; Mode: TadsPositionMode); overload;

    {** Returns True if the sprite occupies the given coordinate, False
     otherwise. }
    function Contains(Point: TPoint): Boolean;

    {** Creates and returns a copy of this object. }
    function Clone: TadsSprite;

    {** This constructs a sprite with one, unanimated, state only, using the
     supplied image. The sprite will own the supplied image and destoy it when
     the sprite itself is destroyed. }
    constructor Create(aSurface: PSDL_Surface; aImage: TadsImage;
      const aDescription: String); virtual;

    {** When a sprite is constructed, its picture will be loaded from the given
     file. In addition to the graphic file, there may be an inifile with the
     same name, but extension '.ini'. A lot of information in that file is read
     by the TadsImage object that handles the actual image, and in addition to
     that the TadsSprite class can read information about the sprite's different
     states according to the following information:

     [state_1]
     ID=1
     FirstIndex=1
     LastIndex=5
     Delay=5
     AnimationMode=forward

     [state_2]
     ID=2
     FirstIndex=6
     LastIndex=10
     Delay=5
     AnimationMode=backward

     [state_3]
     ID=3
     FirstIndex=11
     LastIndex=11
     Delay=0
     AnimationMode=forwardandbackward

     etc.

     There may be any number of states in the file, but the first state must be
     named "state_1" and then the states must be increased by one for each new
     section ("state_2", "state_3" etc.). The ID parameter is the identifier for
     the given state, i.e. the value that should be assigned to StateID if the
     state should be selected. FirstIndex and LastIndex are the first and last
     index of the frames (see TadsImage) through which the sprite should animate
     when in this state. Delay is the number of ticks before increasing the
     frame counter. }
    constructor CreateFromFile(aSurface: PSDL_Surface; const FileName: String); virtual;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
  end; {class}

implementation

uses
  Math,
  SysUtils,
  IniFiles,
  adsLoggerClass;

{---------- TadsSprite --------------------------------------------------------}

constructor TadsSpriteState.Create(const aDescription: String; aID: Integer);
begin
  Description := aDescription;
  ID := aID;
end; {constructor}

{---------- TadsSprite --------------------------------------------------------}

function TadsSprite.IndexOfState(ID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to StateList.Count-1 do
    if TadsSpriteState(StateList[i]).ID = ID then
    begin
      Result := i;
      Break;
    end; {if}
end; {function}

procedure TadsSprite.SetStateID(const Value: Integer);
var
  Index: Integer;
  FrameOffset: Integer;
begin
  FrameOffset := Frame - CurrentState.FirstIndex;
  if Value <> FStateID then
  begin
    Index := IndexOfState(Value);
    if Index <> -1 then
    begin
       FStateID := Value;
       StateIndex := Index;
       Ticks := 0;
       if IndexedStateTransition then
         Frame := Min(CurrentState.FirstIndex + FrameOffset, CurrentState.LastIndex)
       else
         Frame := CurrentState.FirstIndex;
    end {if}
    else
      LogWarning(Self, Format('SetStateID called with unknown ID %d', [Value]));
  end; {if}
end; {procedure}

function TadsSprite.CurrentState: TadsSpriteState;
begin
  Result := TadsSpriteState(StateList[StateIndex]);
end; {function}

function TadsSprite.GetHeight: Integer;
begin
  Result := FImage.FrameHeight;
end; {function}

function TadsSprite.GetWidth: Integer;
begin
  Result := FImage.FrameWidth;
end; {function}

procedure TadsSprite.SetAlpha(const Value: Byte);
begin
  FAlpha := Value;
  SDL_SetAlpha(FImage.Surface, SDL_SRCALPHA, Value);
end; {procedure}

procedure TadsSprite.AddState(ID, FirstIndex, LastIndex, Delay: Integer;
  AnimationMode: TadsAnimationMode);
var
  Index: Integer;
  State: TadsSpriteState;
begin
  Index := IndexOfState(ID);
  if Index = -1 then
    State := TadsSpriteState.Create(Description, ID)
  else
    State := TadsSpriteState(StateList[Index]);
  State.FirstIndex := FirstIndex;
  State.LastIndex := LastIndex;
  State.Delay := Delay;
  State.AnimationMode := AnimationMode;
  if Index = -1 then
  begin
    StateList.Add(State);
    StateID := ID;
  end; {if}
end; {procedure}

procedure TadsSprite.RestoreBackground;
var
  Src, Dest: SDL_Rect;
begin
  Dest.x := Position.X;
  Dest.y := Position.Y;
  Dest.w := FImage.FrameWidth;
  Dest.h := FImage.FrameHeight;
  Src.x := 0;
  Src.y := 0;
  Src.w := Dest.w;
  Src.h := Dest.h;
  SDL_BlitSurface(CoveredBackground, @Src, Surface, @Dest);
end; {procedure}

procedure TadsSprite.Animate;
begin
  Inc(Ticks);
  if Ticks > CurrentState.Delay then
  begin
    Ticks := 0;
    case AnimationDirection of
      adForward:
      begin
        Inc(Frame);
        if Frame > CurrentState.LastIndex then
          if CurrentState.AnimationMode = amForward then
            Frame := CurrentState.FirstIndex
          else
          begin
            Frame := CurrentState.LastIndex;
            AnimationDirection := adBackward;
          end; {else}
      end; {adForward}
      adBackward:
      begin
        Dec(Frame);
        if Frame < CurrentState.FirstIndex then
          if CurrentState.AnimationMode = amBackward then
            Frame := CurrentState.LastIndex
          else
          begin
            Frame := CurrentState.FirstIndex;
            AnimationDirection := adForward;
          end; {if}
      end; {adBackward}
    end; {case}
  end; {if}
end; {procedure}

procedure TadsSprite.Update;
begin
  ;
end; {procedure}

procedure TadsSprite.StoreBackground;
var
  Rect: SDL_Rect;
begin
  if (Position.X <> PreviousX) or (Position.Y <> PreviousY) or AlwaysStore then
  begin
    Rect.x := Position.X;
    Rect.y := Position.Y;
    Rect.w := FImage.FrameWidth;
    Rect.h := FImage.FrameHeight;
    SDL_BlitSurface(Surface, @Rect, CoveredBackground, nil);
    PreviousX := Position.X;
    PreviousY := Position.Y;
  end; {if}
end; {procedure}

procedure TadsSprite.Draw;
var
  Rect: SDL_Rect;
begin
  Rect.x := Position.X;
  Rect.y := Position.Y;
  Rect.w := FImage.FrameWidth;
  Rect.h := FImage.FrameHeight;
  SDL_BlitSurface(FImage.Surface, FImage.ImageRect[Frame], Surface, @Rect);
end; {procedure}

procedure TadsSprite.Move(X, Y: Integer);
begin
  PositionX := PositionX + X;
  PositionY := PositionY + Y;
  FPosition.X := FPosition.X + X;
  FPosition.Y := FPosition.Y + Y;
end; {procedure}

procedure TadsSprite.Place(X, Y: Integer; Mode: TadsPositionMode);
begin
  PositionX := X;
  PositionY := Y;
  PositionMode := Mode;
  case PositionMode of
    pmTopLeft:
    begin
      FPosition.X := PositionX;
      FPosition.Y := PositionY;
    end; {pmTopLeft}
    pmTop:
    begin
      FPosition.X := PositionX - Width div 2;
      FPosition.Y := PositionY;
    end; {pmTopLeft}
    pmTopRight:
    begin
      FPosition.X := PositionX - Width;
      FPosition.Y := PositionY;
    end; {pmTopLeft}
    pmRight:
    begin
      FPosition.X := PositionX - Width;
      FPosition.Y := PositionY - Height div 2;
    end; {pmTopLeft}
    pmBottomRight:
    begin
      FPosition.X := PositionX - Width;
      FPosition.Y := PositionY - Height;
    end; {pmTopLeft}
    pmBottom:
    begin
      FPosition.X := PositionX - Width div 2;
      FPosition.Y := PositionY - Height;
    end; {pmTopLeft}
    pmBottomLeft:
    begin
      FPosition.X := PositionX;
      FPosition.Y := PositionY - Height;
    end; {pmTopLeft}
    pmLeft:
    begin
      FPosition.X := PositionX;
      FPosition.Y := PositionY - Height div 2;
    end; {pmTopLeft}
  else
    FPosition.X := PositionX - Width div 2;
    FPosition.Y := PositionY - Height div 2;
  end; {case}
end; {procedure}

procedure TadsSprite.Place(Coordinates: TPoint; Mode: TadsPositionMode);
begin
  Place(Coordinates.X, Coordinates.Y, Mode);
end; {procedure}

function TadsSprite.Contains(Point: TPoint): Boolean;
var
  Color: Cardinal;
begin
  Result := (Point.X >= FPosition.X) and (Point.X <= FPosition.X + Width) and
    (Point.Y >= FPosition.Y) and (Point.Y <= FPosition.Y + Height);
  if Result and FImage.Transparent then
  begin
    Color := FImage.Pixel[Point.X - FPosition.X, Point.Y - FPosition.Y];
    Result := Color <> SDL_MapRGB(FImage.Surface.format, FImage.Transparent_R,
      FImage.Transparent_G, FImage.Transparent_B);
  end; {if}
end; {function}

procedure TadsSprite.TransferSettings(Dest: TadsSprite);
var
  i: Integer;
  State: TadsSpriteState;
begin
  for i := 1 to StateList.Count-1 do
  begin
    State := TadsSpriteState(StateList[i]);
    Dest.AddState(State.ID, State.FirstIndex, State.LastIndex, State.Delay,
      State.AnimationMode);
  end; {for}
  Dest.Place(PositionX, PositionY, PositionMode);
  Dest.StateID := FStateID;
  Dest.IndexedStateTransition := FIndexedStateTransition;
  Dest.Ticks := Ticks;
  Dest.Frame := Frame;
end; {procedure}

function TadsSprite.Clone: TadsSprite;
begin
  Result := TadsSprite.Create(Surface, FImage.Clone, Description);
  TransferSettings(Result);
end; {function}

constructor TadsSprite.Create(aSurface: PSDL_Surface; aImage: TadsImage;
  const aDescription: String);
begin
  Description := aDescription;
  StateList := TObjectList.Create;
  FImage := aImage;
  if Assigned(FImage.Surface) then
  begin
    CoveredBackground := SDL_DisplayFormat(FImage.Surface);
    SDL_SetColorKey(CoveredBackground, 0, 0);
    Ticks := 0;
    Frame := 0;
    PreviousX := -MAXINT;
    PreviousY := MAXINT;
    PositionX := 0;
    PositionY := 0;
    PositionMode := pmTopLeft;
    Surface := aSurface;
    FAlwaysStore := False;
    FIndexedStateTransition := False;
    AddState(0, 0, FImage.FrameCount-1, 0, amForward);
  end {if}
  else
    LogError(Self, 'aImage has no surface in TadsSprite.Create');
end; {constructor}

constructor TadsSprite.CreateFromFile(aSurface: PSDL_Surface; const FileName: String);

  function StringToAnimationMode(const S: String): TadsAnimationMode;
  begin
    if AnsiSameText(S, 'forward') then
      Result := amForward
    else if AnsiSameText(S, 'backward') then
      Result := amBackward
    else
      Result := amForwardAndBackward;
  end; {function}

var
  State: String;
  IniFile: TIniFile;
  StateCounter: Integer;
  AnimationMode: TadsAnimationMode;
  ID, FirstIndex, LastIndex, Delay: Integer;
begin
  Description := FileName;
  StateList := TObjectList.Create;
  FImage := TadsImage.CreateFromFile(FileName);
  if Assigned(FImage.Surface) then
  begin
    CoveredBackground := SDL_DisplayFormat(FImage.Surface);
    SDL_SetColorKey(CoveredBackground, 0, 0);
    Ticks := 0;
    Frame := 0;
    PreviousX := -MAXINT;
    PreviousY := MAXINT;
    PositionX := 0;
    PositionY := 0;
    PositionMode := pmTopLeft;
    Surface := aSurface;
    FAlwaysStore := False;
    FIndexedStateTransition := False;
    AddState(0, 0, FImage.FrameCount-1, 0, amForward); {If no .ini file info}
    IniFile := TIniFile.Create(ChangeFileExt(FileName, '.ini'));
    try
      StateCounter := 1;
      State := Format('state_%d', [StateCounter]);
      while IniFile.SectionExists(State) do
      begin
        ID := IniFile.ReadInteger(State, 'ID', 0);
        FirstIndex := IniFile.ReadInteger(State, 'FirstIndex', 1);
        LastIndex := IniFile.ReadInteger(State, 'LastIndex', 1);
        Delay := IniFile.ReadInteger(State, 'Delay', 0);
        AnimationMode := StringToAnimationMode(IniFile.ReadString(State,
          'AnimationMode', 'forward'));
        AddState(ID, FirstIndex-1, LastIndex-1, Delay, AnimationMode);
        Inc(StateCounter);
        State := Format('state_%d', [StateCounter]);
      end; {while}
    finally
      IniFile.Free;
    end; {finally}
  end {if}
  else
    LogError(Self, Format('Failed to create sprite from file %s', [FileName]));
end; {constructor}

procedure TadsSprite.BeforeDestruction;
begin
  inherited;
  if Assigned(OnDestroy) then
    OnDestroy(Self);
end; {procedure}

destructor TadsSprite.Destroy;
begin
  FImage.Free;
  StateList.Clear;
  StateList.Free;
  SDL_FreeSurface(CoveredBackground);
  inherited;
end; {destructor}

end.
