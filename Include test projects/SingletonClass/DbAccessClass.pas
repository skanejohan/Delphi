unit DbAccessClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, SingletonClass;

type
  TDbAccess = class(TSingleton)
  protected
    function GetDatabaseVersion: String; virtual;
  public
    property DatabaseVersion: String read GetDatabaseVersion;
    class function Instance: TDbAccess;
  end;

implementation

function TDbAccess.GetDatabaseVersion: String;
begin
  Result := 'Version unknown';
end;

class function TDbAccess.Instance: TDbAccess;
begin
  Result := inherited Instance as TDbAccess;
end;

end.

