unit SpecificDbAccessClasses;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, DbAccessClass;

type
  TNexusAccess = class(TDbAccess)
  protected
    function GetDatabaseVersion: String; override;
  end;

  TMySQLAccess = class(TDbAccess)
  protected
    function GetDatabaseVersion: String; override;
  end;

implementation

function TNexusAccess.GetDatabaseVersion: String;
begin
  Result := 'NexusDB version 3.0';
end;

function TMySQLAccess.GetDatabaseVersion: String;
begin
  Result := 'MySQL version 5.5';
end;

end.

