program adsGameClassesTest;

uses
  SysUtils,
  adsGame,
  adsLoggerClass,
  adsGameTestClasses;

var
  Game: TadsGame;
  GameStates: TadsGameStates;
begin
  try
    Game := TadsGame.Create(800, 600, False, False);
    GameStates := TMyGameStates.Create(Game);
    Game.Run(GameStates);
    GameStates.Free;
    Game.Free;
  except
    on E: Exception do
      LogError(nil, Format('Exception %s with message "%s"',
        [E.ClassName, E.Message]));
  end; {except}
end.
