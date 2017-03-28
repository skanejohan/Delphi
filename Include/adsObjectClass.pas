unit adsObjectClass;

{
  © 2007-2010 Aston Design Studio

  This unit implements TadsObject which acts as the base class for several ADS
  classes. If the compiler directive CHECKMEMORY is not present, the class
  doesn't add any functionality to TObject. If CHECKMEMORY is present, the
  application will automatically log information about memory usage to the
  standard Logger object. When the application exits, it will log any memory
  leaks in ADS classes, i.e. objects of an ADS type that have been created but
  not freed. Note that this is not a regular memory checker, since it does not
  check memory for any non-ADS classes. If you set the LogCreateAndDestroy
  variable to True, it will also log information each time an ADS object is
  created or freed. 
}

interface

uses
  adsLoggerClass;

type
  TadsObject = class(TObject)
  protected
    Description: String;
  {$ifdef CHECKMEMORY}
  protected
    function Identifier: String;
    function ClassNameAndIdentifier: String;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  {$endif}
  end; {class}

var
  LogCreateAndDestroy: Boolean = False;

{$ifdef ADSOBJECTTEST}
procedure ClearObjects;
{$endif}

implementation

{$ifdef CHECKMEMORY}
{$ifndef NOLOGGING}

uses
  Classes,
  SysUtils;

var
  Objects: TList;

function TadsObject.Identifier: String;
begin
  if Description = '' then
    Result := '(' + Self.ClassName + ')'
  else
    Result := ExtractFileName(Description);
end; {function}

function TadsObject.ClassNameAndIdentifier: String;
begin
  Result := Self.ClassName;
  if Description <> '' then
    Result := Result + ' - "' + ExtractFileName(Description) + '"';
end; {function}

procedure TadsObject.AfterConstruction;
begin
  inherited;
  Objects.Add(Self);
  if LogCreateAndDestroy then
    Logger.Log(Self, ltMemDebug, 'Create: ' + Identifier);
end; {procedure}

procedure TadsObject.BeforeDestruction;
begin
  inherited;
  if (Objects.Remove(Self) > -1) and LogCreateAndDestroy then
    Logger.Log(Self, ltMemDebug, 'Destroy: ' + Identifier);
end; {procedure}

procedure ClearObjects;
begin
  while Objects.Count > 0 do
    with TadsObject(Objects[0]) do
    begin
      Logger.Log(nil, ltMemError, 'Memory leak: ' + ClassNameAndIdentifier);
      Objects.Delete(0);
    end; {with}
end{procedure};

initialization
  Objects := TList.Create;

finalization
  ClearObjects;
  Objects.Free;

{$endif}
{$endif}

end.
