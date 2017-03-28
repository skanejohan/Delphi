unit VersionManagementAccessClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, SingletonClass;

type

  { TVersionManagementAccess }

  TVersionManagementAccess = class(TSingleton)
  protected
    FName: String;
  public
    property Name: String read FName;
    class function Instance: TVersionManagementAccess;
    constructor Create; override;
  end;

implementation

constructor TVersionManagementAccess.Create;
begin
  inherited;
  FName := 'Name unknown';
end;

class function TVersionManagementAccess.Instance: TVersionManagementAccess;
begin
  Result := inherited Instance as TVersionManagementAccess;
end;

end.

