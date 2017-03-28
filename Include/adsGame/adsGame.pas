unit adsGame;

{
  © 2007-2010 Aston Design Studio

  The TadsGameState class represents a game state, e.g. "playing", "displaying
  menu", "displaying highscore list", "introduction", "paused" etc. Do not
  create instances of this class, instead derive new classes; TGameStatePlaying,
  TGameStateMenu etc.

  The TadsGameStates class is used to manage a set of game states and the order
  in which they are executed.

  The TadsGame class is the main class for a game.
}

interface

uses
  Classes,
  Contnrs,
  SDL,
  SDL_image,
  adsObjectClass,
  adsImageClass,
  adsLoggerClass,
  adsSpriteClass,
  adsTextClass;

type
  {Forward declarations}
  TadsGame = class;
  TadsGameState = class;
  TadsGameStateClass = class of TadsGameState;

  {** Used to connect a game mode with a game state. }
  TModeState = record
    Mode: Integer;
    State: TadsGameStateClass;
  end; {record}
  PModeState = ^TModeState;

  {** Used to represent the current state of the mouse.}
  TMouse = record
    X, Y: Word;
    Moved: Boolean;
    LeftDown: Boolean;
    LeftPressed: Boolean;
    LeftReleased: Boolean;
    RightDown: Boolean;
    RightPressed: Boolean;
    RightReleased: Boolean;
  end; {record}

  TBackgroundOverlayMode = (boActive, boRemoved, boInactive);

  TadsGameState = class(TadsObject)
  private
    FLeaveState: Boolean;
    FRepaintAllWhenLeavingState: Boolean;
    FAlpha: Byte;
    FBackgroundChanged: Boolean;
    procedure SetAlpha(Value: Byte);
    function GetLeaveState: Boolean;
  protected
    FQuit: Boolean;
    Game: TadsGame;
    Delay: Cardinal;
    StartTime: Cardinal;
    LeaveStateTime: Cardinal;
    FadingIn: Boolean;
    FadeInTime: Cardinal;
    FadingOut: Boolean;
    FadeOutTime: Cardinal;
    Caption: String;
    Background: TadsImage;
    BackgroundOverlay: TadsSprite;
    BackgroundOverlayMode: TBackgroundOverlayMode;
    Mouse: TMouse;
    KeyPressed: Array[SDLK_FIRST..SDLK_EURO] of Boolean;
    KeyDown: Array[SDLK_FIRST..SDLK_EURO] of Boolean;
    KeyReleased: Array[SDLK_FIRST..SDLK_EURO] of Boolean;
    Sprites, RemovedSprites: TList; {Of TadsSprite}
    Texts, RemovedTexts: TList; {Of TadsText}
    TextID: Integer;
    LoggedNumberOfSprites, LoggedNumberOfRemovedSprites: Integer;
    LoggedNumberOfTexts, LoggedNumberOfRemovedTexts: Integer;
    procedure LogSpritesAndTexts;
    procedure TextDestroy(Sender: TObject);
    procedure SpriteDestroy(Sender: TObject);

    {** This method should be called by the inherited class's constructor and
     is used to initialize the game state.
     @param aDelay If <> 0, this is the number of milliseconds until the
      DoLeaveState method is automatically called. This can be used e.g. for a
      state whose purpose is to display the level name or a "Get Ready" message
      for a few seconds before the game actually starts. Please note that the
      default behaviour for a "delayed" state (one where aDelay <> 0) is that
      the user can immediately skip to the next state by pressing space or
      clicking the left mouse button. To change this behaviour, override the
      HandleKeys and/or the HandleMouse methods.
     @CursorVisible Indicates if the mouse cursor should be visible or not.
     @param aCaption This is the text that will be displayed in the window
      caption in this state.
     @param BgFile This is the name of an image file containing the image that
     should be displayed as a background in this state.
     @param aFadeInTime If <> 0, this is the number of millseconds the state
      will "fade in", i.e. go from completely black to "fully drawn"
     @param FadeOutTime If <> 0, this is the number of millseconds the state
      will "fade out", i.e. go from "fully drawn" to completely black. For a
      delayed state, the state will start to fade out this number of
      milliseconds before the time specified in the Delay parameter. Fo a non-
      delayed state, the state will start to fade out when the DoLeaveState
      method is called, and the given number of milliseconds later, the game
      state will actually be left. }
    procedure Initialize(aDelay: Cardinal; CursorVisible: Boolean=True;
      const aCaption: String=''; const BgFile: String='';
      aFadeInTime: Cardinal = 0; aFadeOutTime: Cardinal=0);

    {** This method is called by TadsGame to ensure that the background overlay
     will act correctly, in order to set the alpha value. }
    procedure ApplyAlphaValue;

    {** Override this method to take action when certain keys are pressed, held
     down or released. Index the KeyPressed, KeyDown and KeyReleased arrays by
     the ASCII code for the selected key. SDL defines constants (SDLK_...) for
     all keys that may be used. There are two actions by default: if the space
     key is pressed when in a delayed state, the next state will be entered and
     if Alt + Enter is pressed, the game will switch between windowed mode and
     full-screen mode (assuming the game is configured to allow that; see
     TadsGame.Create). This method is called once per frame. }
    procedure HandleKeys; virtual;

    {** Override this method to take action when the user does something with
     the mouse. Access the Mouse variable for information about the current
     state of the mouse. The Default action is to leave a delayed state if the
     left button is pressed. This method is called once per frame. }
    procedure HandleMouse; virtual;

    {** Override this method to take action when certain SDL events occur. Note
     that events for the mouse and keyboard are generated here, but it is often
     easier to use the HandleKeys and HandleMouse methods for this. This method
     is called a number of times each frame; once per event. The default action
     is to set the Quit property to True if the SDL_QUITEV appears (the user
     requested a quit e.g. by closing the window). }
    procedure HandleEvent(Event: TSDL_Event); virtual;

  public

    {** If this property has been set, it indicates that the TGame object will
     go to the next state at the end of this frame. }
    property LeaveState: Boolean read GetLeaveState;

    {** When leaving the state, this property determines if the system should
     clear the screen before drawing it again. If the background hasn't changed,
     set it to False to avoid unnecessary flicker. Default is True. }
    property RepaintAllWhenLeavingState: Boolean
      read FRepaintAllWhenLeavingState write FRepaintAllWhenLeavingState;

    {** If this property is set to True, the game loop will be exited and the
     application terminate.}
    property Quit: Boolean read FQuit;

    {** Sets the alpha value for the scene, i.e. the "darkness". Set this to 255
     to draw a "normal" scene, set it to 0 to draw a completely black scene or
     to a value in between for a darker scene. This value is set automatically
     when fading in or out. }
    property Alpha: Byte read FAlpha write SetAlpha;

    {** todo comment}
    property BackgroundChanged: Boolean read FBackgroundChanged;

    {** Calls the RestoreBackground method for all sprites and texts, to restore
     the background behind the sprite. This method may be overridden in a
     derived class, if there are additional objects for which this should be
     done. }
    procedure RestoreBackground; virtual;

    {** Polls the SDL event list, and calls HandleEvent zero or more times, and
     calls HandleMouse and HandleKeys once. }
    procedure Input;

    {** Performs sprite animation and checks if it is time, for a delayed state,
     to leave the state. This method may be overridden in a derived class, e.g.
     to move sprites or to leave the state based on some other condition. }
    procedure Update; virtual;

    {** Calls the StoreBackground method for all sprites and texts, to store
     the background at the position where the object will later be drawn. This
     method may be overridden in a derived class, if there are additional
     objects for which this should be done. }
    procedure StoreBackground; virtual;

    {** Draws the background. }
    procedure DrawBackground;

    {** Draws the scene's objects (sprites and texts). This method may be
     overridden in a derived class, if there are additional objects that need
     to be drawn. }
    procedure DrawObjects;

    {** Call this method when it is time to leave the state. This will set the
     LeaveState property. }
    procedure DoLeaveState;

    {** Add a sprite to the scene. All sprites that have been added will
     automatically be drawn and animated. To change the order in which the
     sprites are drawn, override the OrderSpritesAndTexts method. }
    procedure AddSprite(aSprite: TadsSprite);

    {** Remove a sprite from the scene. If the sprite is found, it is removed
     and the function returns True. If not, the function returns False. Note
     that when this method is called, the sprite is added to the RemovedSprites
     list, so that the background behind it can be restored the next time the
     state restores all backgrounds (after which it is removed from the
     RemovedSprites list automatically). Note that the sprite is not destroyed;
     this needs to be done elsewhere. }
    function RemoveSprite(aSprite: TadsSprite): Boolean;

    {** Removes all sprites from the scene. Note that the sprites are not
     destroyed; this needs to be done elsewhere. }
    procedure ClearSprites;

    {** Add a text to the scene. All added texts will automatically be drawn. To
     change the order in which the texts are drawn, override the
     OrderSpritesAndTexts method. }
    procedure AddText(Text: TadsText);

    {** Remove a text from the scene. If the text is found, it is removed and
     the function returns True. If not, the function returns False. Note that
     when this method is called, the texts is added to the RemovedTexts list, so
     that the background behind it can be restored the next time the state
     restores all backgrounds (after which it is removed from the RemovedTexts
     list automatically). Note that the text is not destroyed; this needs to be
     done elsewhere. }
    function RemoveText(Text: TadsText): Boolean;

    {** Removes all texts from the scene. Note that the texts are not destroyed;
     this needs to be done elsewhere. }
    procedure ClearTexts;

    {** This method is called before the scene is drawn and can be overridden to
     determine the order in which the sprites are drawn. When drawn, the sprite
     at position [0] is drawn first, then the one at [1] etc. After all sprites
     are drawn, the texts are drawn. }
    procedure OrderSpritesAndTexts; virtual;

    {** The constructor should be overridden in a derived class, to call the
     Initialize method with appropriate parameters. }
    constructor Create(aGame: TadsGame); virtual;
    destructor Destroy; override;
  end; {class}

  TadsGameStates = class(TadsObject)
  private
    FGameMode: Integer;
    FGameState: TadsGameState;
  protected
    Game: TadsGame;
    ModeStates: TList; {Of PModeState}

    {** Selects a new state. Override this method e.g. to set properties in a
     derived game state object after it has been created. }
    procedure SetGameMode(const Value: Integer); virtual;

    {** This method should be implemented in a derived class, where it should be
     used to determine the game state that should replace this, by setting the
     GameMode property. }
    procedure EnterNextState(OldState: TadsGameState); virtual; abstract;

    {** This method should be called, in the derived class's constructor, once
     per game state that should be available. It is used to associate a certain
     class with a certain ID, something which is later used when the GameMode
     property is set. }
    procedure AddGameState(GameMode: Integer; GameStateClass: TadsGameStateClass);

  public

    {** Read-only access to the current game state object. }
    property GameState: TadsGameState read FGameState;

    {** Setting this property causes the current game state object to be freed,
     and a new one (of the type corresponding to the property value) to be
     created. }
    property GameMode: Integer read FGameMode write SetGameMode;

    {** Override this constructor so that it, after calling "inherited", calls
     AddGameState for all game states that should be available and then sets
     the GameMode property to select the initial state. }
    constructor Create(aGame: TadsGame);
    destructor Destroy; override;
  end; {class}

  TadsGame = class(TadsObject)
  private
    FFPS: Cardinal;
    FTargetFPS: Cardinal;
    FFPSFactor: Double;
    FVideoOK: Boolean;
    FScreen: PSDL_Surface;
  protected
    Frames: Cardinal;
    TickCounter: Cardinal;
    Initialized: Boolean;
    Width, Height: Integer;
    GameStates: TadsGameStates;
    FullScreen: Boolean;
    WindowStateChanged: Boolean;
    AllowFullScreenToggle: Boolean;
    function CreateScreen: Boolean;
    function Initialize: Boolean;
    procedure CalculateFps;
  public

    {** The current number of frames per second.}
    property FPS: Cardinal read FFPS;

    {** The factor between the desired frame rate (TargetFPS) and the actual one
     (FPS). If FPS is higher than the target frame rate, this value will be
     lower than 1. If FPS is lower than the target frame rate, this number will
     be larger than 1. Multiply e.g. movements by this factor to get frame rate
     independent movement. }
    property FPSFactor: Double read FFPSFactor;

    {** The desired frame rate. Default is 100. }
    property TargetFPS: Cardinal read FTargetFPS write FTargetFPS;

    {** This property is True if the video was initialized properly. }
    property VideoOK: Boolean read FVideoOK;

    {** Access to the surface representing the screen (the visible area).}
    property Screen: PSDL_Surface read FScreen;

    {** Run the game loop. Once this procedure finishes, the game is over and
     the game typically terminates. }
    procedure Run(aGameStates: TadsGameStates);

    {** Toggles between windowed and full-screen mode, it the constructor's
     aAllowFullScreenToggle parameter was True, otherwise does nothing. }
    procedure ToggleFullScreen;

    {** The constructor takes the following parameters:
     @param aWidth The width, in pixels, of the visible game area.
     @param aHeight The height, in pixels, of the visible game area.
     @param aFullScreen If True, the game will start in full-screen mode,
      otherwise it will start in windowed mode.
     @param aAllowFullScreenToggle If True, calling the ToggleFullScreen method
      will switch between windowed mode and full-screen mode. If False, the
      ToggleFullScreen method will do nothing. }
    constructor Create(aWidth, aHeight: Integer; aFullScreen,
      aAllowFullScreenToggle: Boolean);
    destructor Destroy; override;
  end; {class}

implementation

uses
  SysUtils, Dialogs, IniFiles;

const
  TicksPerSecond: Cardinal = 1000;

{---------- TadsGameState -----------------------------------------------------}

procedure TadsGameState.SetAlpha(Value: Byte);
begin
  FAlpha := Value;
  if Assigned(BackgroundOverlay) then
    BackgroundOverlay.Alpha := 255-Value;
end; {procedure}

function TadsGameState.GetLeaveState: Boolean;
begin
  Result := FLeaveState and not FadingOut;
end; {function}

procedure TadsGameState.LogSpritesAndTexts;
begin
  if (LoggedNumberOfSprites <> Sprites.Count) or
    (LoggedNumberOfRemovedSprites <> RemovedSprites.Count) or
    (LoggedNumberOfTexts <> Texts.Count) or
    (LoggedNumberOfRemovedTexts <> RemovedTexts.Count) then
  begin
    LogDebug(Self, Format('Sprites: %d + %d removed, texts: %d + %d removed',
      [Sprites.Count, RemovedSprites.Count, Texts.Count, RemovedTexts.Count]));
    LoggedNumberOfSprites := Sprites.Count;
    LoggedNumberOfRemovedSprites := RemovedSprites.Count;
    LoggedNumberOfTexts := Texts.Count;
    LoggedNumberOfRemovedTexts := RemovedTexts.Count;
  end; {if}
end; {procedure}

procedure TadsGameState.TextDestroy(Sender: TObject);
begin
  Texts.Remove(Sender);
  RemovedTexts.Remove(Sender);
end; {procedure}

procedure TadsGameState.SpriteDestroy(Sender: TObject);
begin
  Sprites.Remove(Sender);
  RemovedSprites.Remove(Sender);
end; {procedure}

procedure TadsGameState.Initialize(aDelay: Cardinal; CursorVisible: Boolean=True;
  const aCaption: String=''; const BgFile: String=''; aFadeInTime: Cardinal = 0;
  aFadeOutTime: Cardinal=0);
var
  boImage: TadsImage;
begin
  Delay := aDelay;
  StartTime := SDL_GetTicks;
  FadeInTime := aFadeInTime;
  if FadeInTime > 0 then Alpha := 0;
  FadeOutTime := aFadeOutTime;
  LeaveStateTime := StartTime + Delay;
  Caption := aCaption;
  FreeAndNil(Background);
  if BGFile <> '' then
  begin
    Background := TadsImage.CreateFromFile(BgFile);
    boImage := TadsImage.Create('overlay', Background.Surface, False, 0, 0, 0);
    SDL_FillRect(boImage.Surface, nil, 0);
    BackgroundOverlay := TadsSprite.Create(Game.Screen, boImage, 'overlay');
    //BackgroundOverlay.AlwaysStore := True;
  end; {if}
  if CursorVisible then SDL_ShowCursor(1) else SDL_ShowCursor(0);
  LoggedNumberOfSprites := 0;
  LoggedNumberOfRemovedSprites := 0;
  LoggedNumberOfTexts := 0;
  LoggedNumberOfRemovedTexts := 0;
  FBackgroundChanged := True;
end; {procedure}

procedure TadsGameState.ApplyAlphaValue;
begin
  if Alpha < 255 then
    BackgroundOverlayMode := boActive
  else if BackgroundOverlayMode = boRemoved then
    BackgroundOverlayMode := boInactive
  else
    BackgroundOverlayMode := boRemoved;
end; {procedure}

procedure TadsGameState.HandleKeys;
begin
  if (KeyDown[SDLK_LALT] or KeyDown[SDLK_RALT]) and KeyPressed[SDLK_RETURN] then
    Game.ToggleFullScreen;
  if (Delay <> 0) and KeyPressed[SDLK_SPACE] then
    DoLeaveState;
end; {procedure}

procedure TadsGameState.HandleMouse;
begin
  if (Delay <> 0) and Mouse.LeftPressed then
    DoLeaveState;
end; {procedure}

procedure TadsGameState.HandleEvent(Event: TSDL_Event);
begin
  case Event.type_ of
    SDL_QUITEV:
      FQuit := True;
  end; {case}
end; {procedure}

procedure TadsGameState.RestoreBackground;
var
  i: Integer;
begin
  LogSpritesAndTexts;
  for i := 0 to Sprites.Count-1 do
    TadsSprite(Sprites[i]).RestoreBackground;
  for i := 0 to RemovedSprites.Count-1 do
    TadsSprite(RemovedSprites[i]).RestoreBackground;
  for i := 0 to Texts.Count-1 do
    TadsText(Texts[i]).RestoreBackground;
  for i := RemovedTexts.Count-1 downto 0 do
    TadsText(RemovedTexts[i]).RestoreBackground;
  if Assigned(BackgroundOverlay) then
    //if BackgroundOverlayMode in [boActive, boRemoved] then
      BackgroundOverlay.RestoreBackground;
  RemovedSprites.Clear;
  RemovedTexts.Clear;
end; {procedure}

procedure TadsGameState.Input;
var
  Key: Cardinal;
  Event: TSDL_Event;
begin
  Mouse.LeftPressed := False;
  Mouse.LeftReleased := False;
  Mouse.RightPressed := False;
  Mouse.RightReleased := False;
  for Key := SDLK_FIRST to SDLK_EURO do
  begin
    KeyPressed[Key] := False;
    KeyReleased[Key] := False;
  end; {for}
  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_MOUSEMOTION:
      begin
        Mouse.Moved := (Mouse.X <> Event.motion.x) or (Mouse.Y <> Event.motion.y);
        Mouse.X := Event.motion.x;
        Mouse.Y := Event.motion.y;
      end; {SDL_MOUSEMOTION}
      SDL_MOUSEBUTTONDOWN:
      begin
        if Event.button.button = SDL_BUTTON_LEFT then
        begin
          Mouse.LeftPressed := True;
          Mouse.LeftDown := True;
        end; {if}
        if Event.button.button = SDL_BUTTON_RIGHT then
        begin
          Mouse.RightPressed := True;
          Mouse.RightDown := True;
        end; {if}
      end; {SDL_MOUSEBUTTONDOWN}
      SDL_MOUSEBUTTONUP:
      begin
        if Event.button.button = SDL_BUTTON_LEFT then
        begin
          Mouse.LeftReleased := True;
          Mouse.LeftDown := False;
        end; {if}
        if Event.button.button = SDL_BUTTON_RIGHT then
        begin
          Mouse.RightReleased := True;
          Mouse.RightDown := False;
        end; {if}
      end; {SDL:MOUSEBUTTONUP}
      SDL_KEYDOWN:
      begin
        Key := Event.key.keysym.sym;
        KeyPressed[Key] := True;
        KeyDown[Key] := True;
      end; {SDL_KEYDOWN}
      SDL_KEYUP:
      begin
        Key := Event.key.keysym.sym;
        KeyReleased[Key] := True;
        KeyDown[Key] := False;
      end; {SDL_KEYUP}
    end; {case}
    HandleEvent(Event);
  end; {while}
  HandleMouse;
  HandleKeys;
end; {procedure}

procedure TadsGameState.Update;
var
  i: Integer;
  Ticks: Cardinal;
begin
  {Animate and update the sprites}
  for i := 0 to Sprites.Count-1 do
  begin
    TadsSprite(Sprites[i]).Animate;
    TadsSprite(Sprites[i]).Update;
  end; {for}
  Ticks := SDL_GetTicks;
  {Check if we should fade in}
  if (Ticks < StartTime + FadeInTime) then
  begin
    FadingIn := True;
    Alpha := Round(255 * (Ticks - StartTime) / FadeInTime);
  end {if}
  else if FadingIn then
  begin
    FadingIn := False;
    Alpha := 255;
  end {if}
  {Check if we should fade out}
  else if (Ticks > LeaveStateTime - FadeOutTime) and (Ticks < LeaveStateTime) then
  begin
    FadingOut := True;
    Alpha := 255 - Round(255 * (Ticks - (LeaveStateTime - FadeOutTime)) / FadeOutTime);
  end {if}
  else if FadingOut then
  begin
    FadingOut := False;
    Alpha := 0;
  end {if}
  {For a delayed state, check if it is time to leave}
  else if (Delay <> 0) and (Ticks > LeaveStateTime) then
    DoLeaveState;
end; {procedure}

procedure TadsGameState.StoreBackground;
var
  i: Integer;
begin
  for i := 0 to Sprites.Count-1 do
    TadsSprite(Sprites[i]).StoreBackground;
  for i := 0 to Texts.Count-1 do
    TadsText(Texts[i]).StoreBackground;
  if Assigned(BackgroundOverlay) then
    //if BackgroundOverlayMode = boActive then
      BackgroundOverlay.StoreBackground;
end; {procedure}

procedure TadsGameState.DrawBackground;
var
  SrcRect, DestRect: SDL_Rect;
begin
  SDL_WM_SetCaption(PChar(Caption), nil);
  if Assigned(Background) and Assigned(Background.Surface) then
  begin
    SrcRect.x := 0;
    SrcRect.y := 0;
    SrcRect.w := Background.FrameWidth;
    SrcRect.h := Background.FrameHeight;
    DestRect.x := 0;
    DestRect.y := 0;
    DestRect.w := Game.Screen.w;
    DestRect.h := Game.Screen.h;
    SDL_FillRect(Game.Screen, nil, 0);
    SDL_BlitSurface(Background.Surface, @SrcRect, Game.Screen, @DestRect);
  end {if}
  else
   SDL_FillRect(Game.Screen, nil, 0);
  FBackgroundChanged := False;
end; {procedure}

procedure TadsGameState.DrawObjects;
var
  i: Integer;
begin
  for i := 0 to Pred(Sprites.Count) do
    TadsSprite(Sprites[i]).Draw;
  for i := 0 to Pred(Texts.Count) do
    TadsText(Texts[i]).Draw;
  if Assigned(BackgroundOverlay) then
    if BackgroundOverlayMode = boActive then
      BackgroundOverlay.Draw;
end; {procedure}

procedure TadsGameState.DoLeaveState;
begin
  FLeaveState := True;
end; {procedure}

procedure TadsGameState.AddSprite(aSprite: TadsSprite);
begin
  Sprites.Remove(aSprite);
  Sprites.Add(aSprite);
  aSprite.OnDestroy := SpriteDestroy;
end; {procedure}

function TadsGameState.RemoveSprite(aSprite: TadsSprite): Boolean;
begin
  Result := Sprites.Remove(aSprite) >= 0;
  if Result then
    RemovedSprites.Add(aSprite)
  else
    LogWarning(Self, 'RemoveSprite called but sprite not found');
end; {function}

procedure TadsGameState.ClearSprites;
begin
  while Sprites.Count > 0 do
    RemoveSprite(Sprites[0]);
end; {function}

procedure TadsGameState.OrderSpritesAndTexts;
begin
  ;
end; {procedure}

procedure TadsGameState.AddText(Text: TadsText);
begin
  Texts.Remove(Text);
  Texts.Add(Text);
  Text.OnDestroy := TextDestroy;
end; {function}

function TadsGameState.RemoveText(Text: TadsText): Boolean;
begin
  Result := Texts.Remove(Text) >= 0;
  if Result then
    RemovedTexts.Add(Text)
  else
    LogWarning(Self, 'RemoveText called but text not found');
end; {procedure}

procedure TadsGameState.ClearTexts;
begin
  while Texts.Count > 0 do
    RemoveText(Texts[0]);
end; {procedure}

constructor TadsGameState.Create(aGame: TadsGame);
var
  i: Integer;
begin
  Description := '';
  FAlpha := 255;
  Game := aGame;
  Sprites := TList.Create;
  RemovedSprites := TList.Create;
  Texts := TList.Create;
  RemovedTexts := TList.Create;
  Background := nil;
  FQuit := False;
  for i := Low(KeyPressed) to High(KeyPressed) do
  begin
    KeyPressed[i] := False;
    KeyDown[i] := False;
    KeyReleased[i] := False;
  end; {for}
  TextID := 0;
  FRepaintAllWhenLeavingState := True;
  BackgroundOverlay := nil;
  BackgroundOverlayMode := boInactive;
end; {constructor}

destructor TadsGameState.Destroy;
begin
  BackgroundOverlay.Free;
  Background.Free;
  RemovedSprites.Free;
  Sprites.Free;
  RemovedTexts.Free;
  Texts.Free;
  inherited;
end; {destructor}

{---------- TadsGameStates ----------------------------------------------------}

procedure TadsGameStates.SetGameMode(const Value: Integer);
var
  i: Integer;
begin
  FGameMode := Value;
  FGameState.Free;
  for i := 0 to ModeStates.Count-1 do
    if PModeState(ModeStates[i])^.Mode = FGameMode then
    begin
      FGameState := PModeState(ModeStates[i])^.State.Create(Game);
      Break;
    end; {if}
end; {procedure}

procedure TadsGameStates.AddGameState(GameMode: Integer;
  GameStateClass: TadsGameStateClass);
var
  ModeState: PModeState;
begin
  New(ModeState);
  ModeState^.Mode := GameMode;
  ModeState^.State := GameStateClass;
  ModeStates.Add(ModeState);
end; {procedure}

constructor TadsGameStates.Create(aGame: TadsGame);
begin
  Description := '';
  FGameState := nil;
  ModeStates := TList.Create;
  Game := aGame;
end; {constructor}

destructor TadsGameStates.Destroy;
var
  i: Integer;
begin
  for i := 0 to ModeStates.Count-1 do
    Dispose(ModeStates[i]);
  ModeStates.Free;
  FGameState.Free;
  inherited;
end; {destructor}

{---------- TadsGame ----------------------------------------------------------}

function TadsGame.Initialize: Boolean;
begin
  Result := SDL_Init(SDL_INIT_VIDEO) >= 0;
  FVideoOK := SDL_WasInit(SDL_INIT_VIDEO) and SDL_INIT_VIDEO = SDL_INIT_VIDEO;
  FScreen := nil;
  if FVideoOK then Result := CreateScreen;
end; {function}

procedure TadsGame.CalculateFps;
var
  FPSSingle: Single;
  CurrentTicks: Cardinal;
begin
  CurrentTicks := SDL_GetTicks;
  try
    FPSSingle := TicksPerSecond / (CurrentTicks - TickCounter);
  except
    FPSSingle := 1;
  end; {except}
  TickCounter := CurrentTicks;
  try
    FFpsFactor := FTargetFPS / FPSSingle;
  except
    FFpsFactor := 1;
  end; {except}
  FFPS := Trunc(FPSSingle);
end; {procedure}

procedure TadsGame.Run(aGameStates: TadsGameStates);
var
  Continue: Boolean;
begin
  GameStates := aGameStates;
  TickCounter := SDL_GetTicks;
  Continue := True;
  repeat
    GameStates.GameState.RestoreBackground;
    GameStates.GameState.Input;
    GameStates.GameState.Update;
    CalculateFps;
    if GameStates.GameState.BackgroundChanged then
      GameStates.GameState.DrawBackground;
    GameStates.GameState.OrderSpritesAndTexts;
    GameStates.GameState.ApplyAlphaValue;
    GameStates.GameState.StoreBackground;
    GameStates.GameState.DrawObjects;
    if GameStates.GameState.Quit then
      Continue := False;
    if GameStates.GameState.LeaveState then
      GameStates.EnterNextState(GameStates.GameState);
    SDL_Flip(Screen);
  until not Continue;
end; {procedure}

function TadsGame.CreateScreen: Boolean;
begin
  { todo
  if FScreen <> nil then
    FreeMemory(FScreen);
  }
  FScreen := nil;
  // todo not OK with HW-acc, but doesn't return nil?
  if FullScreen then
    FScreen := SDL_SetVideoMode(800, 600, 32, SDL_SWSURFACE or SDL_FULLSCREEN)
  else
  begin
    FScreen := SDL_SetVideoMode(800, 600, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
    if FScreen = nil then
      FScreen := SDL_SetVideoMode(800, 600, 32, SDL_SWSURFACE);
  end; {else}
  Result := Assigned(FScreen);
end; {procedure}

procedure TadsGame.ToggleFullScreen;
begin
  if AllowFullScreenToggle then
  begin
    FullScreen := not FullScreen;
    WindowStateChanged := True;
    CreateScreen;
  end; {if}
  {todo: när jag minimerar så är det inte alltid jag kan ge fönstret fokus igen.}
end; {procedure}

constructor TadsGame.Create(aWidth, aHeight: Integer; aFullScreen,
  aAllowFullScreenToggle: Boolean);
begin
  Width := aWidth;
  Height := aHeight;
  FTargetFPS := 100;
  FullScreen := aFullScreen;
  AllowFullScreenToggle := aAllowFullScreenToggle;
  Initialize;
  Description := '';
  WindowStateChanged := False;
end; {constructor}

destructor TadsGame.Destroy;
begin
  if Initialized then
    SDL_Quit;
  inherited;
end; {destructor}

end.
