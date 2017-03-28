unit SingletonTestFrm2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DbAccessClass, VersionManagementAccessClass;

type
  TSingletonTestForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  SingletonTestForm2: TSingletonTestForm2;

implementation

procedure TSingletonTestForm2.Button1Click(Sender: TObject);
begin
  Caption := TDbAccess.Instance.DatabaseVersion + ' / ' +
    TVersionManagementAccess.Instance.Name;
end;

initialization
  {$I SingletonTestFrm2.lrs}

end.

