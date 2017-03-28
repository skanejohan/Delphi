unit adsObjectTestClasses;

interface

uses
  adsObjectClass;

type
  TadsObject1 = class(TadsObject)
  public
    constructor Create;
  end{class};

  TadsObject2 = class(TadsObject)
  end{class};

implementation

{ TadsObject1 }

constructor TadsObject1.Create;
begin
  inherited;
  Description := 'TADSOBJECT1';
end;

end.
