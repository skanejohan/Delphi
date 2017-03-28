unit adsUtils;

{
  © 2008 Aston Design Studio

  Miscellaneous utility methods.
}

interface

uses
  adsContainers;

type
  TadsUtils = class
  public

    {** Fills the list with integers from Start to Stop, with Steps entries in
     all, as equally spread as possible. }
    class procedure CreateSteps(Start, Stop, Steps: Integer; List: TadsIntegerList);
    
  end; {public}

implementation

class procedure TadsUtils.CreateSteps(Start, Stop, Steps: Integer; List: TadsIntegerList);
var
  i, LastInserted, NextToInsert: Integer;
begin
  List.Clear;
  List.Add(Start);
  LastInserted := Start;
  for i := 0 to Steps-3 do
  begin
    NextToInsert := Round(LastInserted + ((Stop - LastInserted) / (Steps - 1 - i)));
    List.Add(NextToInsert);
    LastInserted := NextToInsert;
  end; {for}
  List.Add(Stop);
end; {class procedure}

end.
