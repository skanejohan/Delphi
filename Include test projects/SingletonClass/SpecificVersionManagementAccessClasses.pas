unit SpecificVersionManagementAccessClasses;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, VersionManagementAccessClass;

type
  TGitAccess = class(TVersionManagementAccess)
  public
    constructor Create; override;
  end;

  TSubversionAccess = class(TVersionManagementAccess)
  public
    constructor Create; override;
  end;

implementation

constructor TGitAccess.Create;
begin
  inherited;
  FName := 'Git';
end;

constructor TSubversionAccess.Create;
begin
  inherited;
  FName := 'Subversion';
end;

end.

