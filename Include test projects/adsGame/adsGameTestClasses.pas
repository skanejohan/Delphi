unit adsGameTestClasses;

{
  © 2007-2010 Aston Design Studio

  Tests the ADS game classes.

  Run the application with compiler directive CHECKMEMORY to see that the
  FPSText object that is incorrectly not destroyed causes an error log in the
  .memory.log file

}

interface

uses
  Contnrs,
  SDL,
  adsSpriteClass,
  adsTextClass,
  adsGame;

type
  {** Base class for all the test states. }
  TMyGameState = class(TadsGameState)
  protected
    FPSText: TadsText;
    FPSTextTicks: Byte;
    Texts: TObjectList; {Of TadsText}
    function CreateText(X, Y: Cardinal; Mode: TadsPositionMode; const Text: String): TadsText;
  public
    procedure Update; override;
    procedure OrderSpritesAndTexts; override;
    constructor Create(aGame: TadsGame); override;
    destructor Destroy; override;
  end; {class}

  {** Tests the default behaviour for a "delayed" game state, i.e. the delay
   itself, and "left-click or space to leave the state". Has no background
   picture. }
  TMyGameState1 = class(TMyGameState)
  public
    constructor Create(aGame: TadsGame); override;
  end; {class}

  {** Tests behaviour for a "delayed" game state where "ESC" leaves the state.
   Displays a background picture. }
  TMyGameState2 = class(TMyGameState)
  protected
    procedure HandleMouse; override;
    procedure HandleKeys; override;
  public
    constructor Create(aGame: TadsGame); override;
  end; {class}

  {** Tests behaviour for a "non-delayed" game state where "ESC" leaves the
   state. Displays a background picture and on top it an animated transparent
   sprite with only one state that moves around. }
  TMyGameState3 = class(TMyGameState)
  protected
    Sprite: TadsSprite;
    VelX, VelY: Integer;
    procedure HandleKeys; override;
  public
    procedure Update; override;
    procedure OrderSpritesAndTexts; override;
    constructor Create(aGame: TadsGame); override;
    destructor Destroy; override;
  end; {class}

  {** Tests behaviour for a "non-delayed" game state where "ESC" leaves the
   state. Displays a sprite that has two states, one (animated) for when the
   mouse pointer is situated above it and another (not animated) for when it is
   not. }
  TMyGameState4 = class(TMyGameState)
  protected
    Sprite: TadsSprite;
    procedure HandleMouse; override;
    procedure HandleKeys; override;
  public
    procedure OrderSpritesAndTexts; override;
    constructor Create(aGame: TadsGame); override;
    destructor Destroy; override;
  end; {class}

  {** Tests behaviour for keyboard and mouse. "ESC" leaves the state. }
  TMyGameState5 = class(TMyGameState)
  protected
    pLeft, dLeft, rLeft: TadsText;
    pRight, dRight, rRight: TadsText;
    pu, du, ru: TadsText;
    pF5, dF5, rF5: TadsText;
    pCTRL, dCTRL, rCTRL: TadsText;
    pLeftCtr, rLeftCtr: Byte;
    pRightCtr, rRightCtr: Byte;
    puCtr, ruCtr: Byte;
    pF5Ctr, rF5Ctr: Byte;
    pCTRLCtr, rCTRLCtr: Byte;
    procedure HandleDown(Value: Boolean; Text: TadsText);
    procedure HandlePressedReleased(Value: Boolean; var Ctr: Byte; Text: TadsText);
    procedure HandleMouse; override;
    procedure HandleKeys; override;
  public
    constructor Create(aGame: TadsGame); override;
  end; {class}

  {** Creates and uses the correct game state types. }
  TMyGameStates = class(TadsGameStates)
  protected
    procedure EnterNextState(OldState: TadsGameState); override;
  public
    constructor Create(aGame: TadsGame);
  end; {class}

implementation

uses SysUtils;

const
  MyGameState1 = 1;
  MyGameState2 = 2;
  MyGameState3 = 3;
  MyGameState4 = 4;
  MyGameState5 = 5;

  asUpLeftActive = 0;
  asUpLeftInactive = 1;
  asUpRightActive = 2;
  asUpRightInactive = 3;

var
  dataDir: String;
  bgFile: String;
  largeFontFile: String;
  smallFontFile: String;
  arrowsFile: String;

{---------- TMyGameState ------------------------------------------------------}

procedure TMyGameState.Update;
begin
  inherited;
  Inc(FPSTextTicks);
  if FPSTextTicks mod 10 = 0 then
    FPSText.SetText(Format('FPS: %d', [Game.FPS]));
end; {procedure}

function TMyGameState.CreateText(X, Y: Cardinal; Mode: TadsPositionMode;
  const Text: String): TadsText;
begin
  Result := TadsText.CreateFromFile(Game.Screen, smallFontFile);
  Result.AlwaysStore := True;
  Result.SetText(Text);
  Result.Place(X, Y, Mode);
  Texts.Add(Result);
end; {procedure}

procedure TMyGameState.OrderSpritesAndTexts;
var
  i: Integer;
begin
  ClearSprites;
  ClearTexts;
  AddText(FPSText);
  for i := 0 to Pred(Texts.Count) do
    AddText(Texts[i] as TadsText);
end; {procedure}

constructor TMyGameState.Create(aGame: TadsGame);
begin
  inherited;
  Texts := TObjectList.Create;
  FPSText := TadsText.CreateFromFile(Game.Screen, largeFontFile);
  FPSText.AlwaysStore := True;
  FPSText.Place(200, 400, pmTopLeft);
  FPSTextTicks := 0;
end; {constructor}

destructor TMyGameState.Destroy;
begin
  {FPSText.Free;} {Will cause four lines in the memory leak log for each tested state}
  Texts.Free;
  inherited;
end; {destructor}

{---------- TMyGameState1 -----------------------------------------------------}

constructor TMyGameState1.Create(aGame: TadsGame);
begin
  inherited;
  Initialize(30000, True, 'TMyGameState1', '');
  CreateText(100, 100, pmTopLeft, 'Waiting 30 seconds.');
  CreateText(100, 120, pmTopLeft, 'Mouse cursor visible.');
  CreateText(100, 140, pmTopLeft, 'Left-click or space to leave.');
  CreateText(100, 160, pmTopLeft, 'Also testing placement below');
  CreateText(200, 250, pmTopLeft, 'TopLeft');
  CreateText(200, 250, pmTopRight, 'TopRight');
  CreateText(200, 250, pmBottomLeft, 'BottomLeft');
  CreateText(200, 250, pmBottomRight, 'BottomRight');
  CreateText(200, 250, pmCenter, '+');
  CreateText(400, 250, pmTop, 'Top');
  CreateText(400, 250, pmRight, 'Right');
  CreateText(400, 250, pmBottom, 'Bottom');
  CreateText(400, 250, pmLeft, 'Left');
end; {constructor}

{---------- TMyGameState2 -----------------------------------------------------}

procedure TMyGameState2.HandleMouse;
begin
  {Default behaviour for a delayed state is that left-click leaves the state,
  but we don't want that.}
end; {procedure}

procedure TMyGameState2.HandleKeys;
begin
  {Default behaviour for a delayed state is that space leaves the state, but we
   want it to be ESC instead.}
  if KeyPressed[SDLK_ESCAPE] then
    DoLeaveState;
end; {procedure}

constructor TMyGameState2.Create(aGame: TadsGame);
begin
  inherited;
  Initialize(5000, False, 'TMyGameState2', bgFile, 0, 1000);
  RepaintAllWhenLeavingState := True;
  CreateText(100, 100, pmTopLeft, 'Waiting 5 seconds.');
  CreateText(100, 120, pmTopLeft, 'No mouse cursor.');
  CreateText(100, 140, pmTopLeft, 'Fades out.');
  CreateText(100, 160, pmTopLeft, 'ESC to leave.');
end; {constructor}

{----------  TMyGameState3 ----------------------------------------------------}

procedure TMyGameState3.HandleKeys;
begin
  {Default behaviour for a non-delayed state, is not to leave the state when a
   key is pressed, but we want ESC to take us to the next state.}
  if KeyPressed[SDLK_ESCAPE] then
    DoLeaveState;
end; {procedure}

procedure TMyGameState3.Update;
begin
  with Sprite do
  begin
    Move(VelX, VelY);
    if Position.X = 0 then VelX := 1;
    if Position.X = 700 then VelX := -1;
    if Position.Y = 0 then VelY := 1;
    if Position.Y = 500 then VelY := -1;
  end; {with}
  inherited;
end; {procedure}

procedure TMyGameState3.OrderSpritesAndTexts;
begin
  inherited;
  AddSprite(Sprite);
end; {procedure}

constructor TMyGameState3.Create(aGame: TadsGame);
begin
  inherited;
  Initialize(0, False, 'TGameState3', bgFile, 1000);
  CreateText(100, 100, pmTopLeft, 'ESC to leave.');
  Sprite := TadsSprite.CreateFromFile(aGame.Screen, arrowsFile);
  Sprite.StateID := 0;
  VelX := 1;
  VelY := 1;
end; {constructor}

destructor TMyGameState3.Destroy;
begin
  Sprite.Free;
  inherited;
end; {destructor}

{---------- TMyGameState4 -----------------------------------------------------}

procedure TMyGameState4.HandleKeys;
begin
  {Default behaviour for a non-delayed state, is not to leave the state when a
   key is pressed, but we want ESC to take us to the next state.}
  if KeyPressed[SDLK_ESCAPE] then
    DoLeaveState;
end; {procedure}

procedure TMyGameState4.HandleMouse;
begin
  {This is where we select the correct sprite state depending on whether the
   mouse points at the sprite or not. }
  if (Mouse.X > 0) and (Mouse.X < 46) and (Mouse.Y > 0) and (Mouse.Y < 44) then
    Sprite.StateID := asUpLeftActive
  else
    Sprite.StateID := asUpRightActive;
end; {procedure}

procedure TMyGameState4.OrderSpritesAndTexts;
begin
  inherited;
  AddSprite(Sprite);
end; {procedure}

constructor TMyGameState4.Create(aGame: TadsGame);
begin
  inherited;
  Initialize(0, True, 'TGameState4', '');
  CreateText(100, 100, pmTopLeft, 'ESC to leave.');
  CreateText(100, 120, pmTopLeft, 'Move the cursor over the arrow.');
  Sprite := TadsSprite.CreateFromFile(aGame.Screen, arrowsFile);
end; {constructor}

destructor TMyGameState4.Destroy;
begin
  Sprite.Free;
  inherited;
end; {destructor}

{---------- TMyGameState5 -----------------------------------------------------}

procedure TMyGameState5.HandleDown(Value: Boolean; Text: TadsText);
begin
  if Value then
    Text.SetText('Yes')
  else
    Text.SetText('No');
end; {procedure}

procedure TMyGameState5.HandlePressedReleased(Value: Boolean; var Ctr: Byte; Text: TadsText);
begin
  if Value then
  begin
    Inc(Ctr);
    Text.SetText(IntToStr(Ctr));
  end; {if}
end; {procedure}

procedure TMyGameState5.HandleKeys;
begin
  inherited;
  HandlePressedReleased(KeyPressed[SDLK_u], puCtr, pu);
  HandleDown(KeyDown[SDLK_u], du);
  HandlePressedReleased(KeyReleased[SDLK_u], ruCtr, ru);
  HandlePressedReleased(KeyPressed[SDLK_F5], pF5Ctr, pF5);
  HandleDown(KeyDown[SDLK_F5], dF5);
  HandlePressedReleased(KeyReleased[SDLK_F5], rF5Ctr, rF5);
  HandlePressedReleased(KeyPressed[SDLK_LCTRL], pCTRLCtr, pCTRL);
  HandleDown(KeyDown[SDLK_LCTRL], dCTRL);
  HandlePressedReleased(KeyReleased[SDLK_LCTRL], rCTRLCtr, rCTRL);
  if KeyPressed[SDLK_ESCAPE] then DoLeaveState;
end; {procedure}

procedure TMyGameState5.HandleMouse;
begin
  inherited;
  HandlePressedReleased(Mouse.LeftPressed, pLeftCtr, pLeft);
  HandleDown(Mouse.LeftDown, dLeft);
  HandlePressedReleased(Mouse.LeftReleased, rLeftCtr, rLeft);
  HandlePressedReleased(Mouse.RightPressed, pRightCtr, pRight);
  HandleDown(Mouse.RightDown, dRight);
  HandlePressedReleased(Mouse.RightReleased, rRightCtr, rRight);
end; {procedure}

constructor TMyGameState5.Create(aGame: TadsGame);
begin
  inherited;
  CreateText(120, 10, pmTopLeft, 'Left');
  CreateText(200, 10, pmTopLeft, 'Right');
  CreateText(280, 10, pmTopLeft, '"u"');
  CreateText(360, 10, pmTopLeft, '"F5"');
  CreateText(440, 10, pmTopLeft, '"CTRL"');
  CreateText(10, 30, pmTopLeft, 'Pressed:');
  CreateText(10, 50, pmTopLeft, 'Down:');
  CreateText(10, 70, pmTopLeft, 'Released:');
  pLeft := CreateText(120, 30, pmTopLeft, '');
  dLeft := CreateText(120, 50, pmTopLeft, '');
  rLeft := CreateText(120, 70, pmTopLeft, '');
  pRight := CreateText(200, 30, pmTopLeft, '');
  dRight := CreateText(200, 50, pmTopLeft, '');
  rRight := CreateText(200, 70, pmTopLeft, '');
  pu := CreateText(280, 30, pmTopLeft, '');
  du := CreateText(280, 50, pmTopLeft, '');
  ru := CreateText(280, 70, pmTopLeft, '');
  pF5 := CreateText(360, 30, pmTopLeft, '');
  dF5 := CreateText(360, 50, pmTopLeft, '');
  rF5 := CreateText(360, 70, pmTopLeft, '');
  pCTRL := CreateText(440, 30, pmTopLeft, '');
  dCTRL := CreateText(440, 50, pmTopLeft, '');
  rCTRL := CreateText(440, 70, pmTopLeft, '');
  pLeftCtr := 0;
  rLeftCtr := 0;
  pRightCtr := 0;
  rRightCtr := 0;
  puCtr := 0;
  ruCtr := 0;
  pF5Ctr := 0;
  rF5Ctr := 0;
  pCTRLCtr := 0;
  rCTRLCtr := 0;
end; {constructor}

{---------- TMyGameStates -----------------------------------------------------}

constructor TMyGameStates.Create(aGame: TadsGame);
begin
  inherited;
  AddGameState(MyGameState1, TMyGameState1);
  AddGameState(MyGameState2, TMyGameState2);
  AddGameState(MyGameState3, TMyGameState3);
  AddGameState(MyGameState4, TMyGameState4);
  AddGameState(MyGameState5, TMyGameState5);
  GameMode := MyGameState1;
end; {constructor}

procedure TMyGameStates.EnterNextState(OldState: TadsGameState);
begin
  case GameMode of
    MyGameState1: GameMode := MyGameState2;
    MyGameState2: GameMode := MyGameState3;
    MyGameState3: GameMode := MyGameState4;
    MyGameState4: GameMode := MyGameState5;
  else
    GameMode := MyGameState1;
  end; {case}
end; {procedure}

initialization
  DataDir := GetCurrentDir + '\Test Data\';
  bgFile := DataDir + 'bg_soccer.png';
  largeFontFile := DataDir + 'font_large.png';
  smallFontFile := DataDir + 'font_small.png';
  arrowsFile := DataDir + 'arrows.png';

end.
