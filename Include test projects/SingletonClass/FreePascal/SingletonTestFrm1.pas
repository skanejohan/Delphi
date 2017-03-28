unit SingletonTestFrm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DbAccessClass, SpecificDbAccessClasses, VersionManagementAccessClass,
  SpecificVersionManagementAccessClasses, SingletonTestFrm2;

type

  { TSingletonTestForm1 }

  TSingletonTestForm1 = class(TForm)
    Button: TButton;
    CreateButton: TButton;
    DestructionLabel: TLabel;
    FreeButton: TButton;
    DbListBox: TListBox;
    CreationLabel: TLabel;
    SingletonListLabel: TLabel;
    VerManListBox: TListBox;
    procedure ButtonClick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure DbListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FreeButtonClick(Sender: TObject);
    procedure VerManListBoxClick(Sender: TObject);
  protected
    procedure LogFromSingleton(const Msg: String);
  end;

var
  SingletonTestForm1: TSingletonTestForm1;

implementation

uses
  SingletonClass{needed only for Loghandler};

procedure TSingletonTestForm1.FormShow(Sender: TObject);
begin
  SingletonTestForm2.Show;
end;

procedure TSingletonTestForm1.FreeButtonClick(Sender: TObject);
begin
  TDbAccess.Instance.Free;
end;

procedure TSingletonTestForm1.ButtonClick(Sender: TObject);
begin
  Caption := TDbAccess.Instance.DatabaseVersion + ' / ' +
    TVersionManagementAccess.Instance.Name;
end;

procedure TSingletonTestForm1.CreateButtonClick(Sender: TObject);
begin
  //TNexusAccess.Create;
  TSingleton.Create;
end;

procedure TSingletonTestForm1.DbListBoxClick(Sender: TObject);
begin
  case DbListBox.ItemIndex of
    0: TDbAccess.TypedInstance(TNexusAccess);
    1: TDbAccess.TypedInstance(TMySQLAccess);
  end;
end;

procedure TSingletonTestForm1.VerManListBoxClick(Sender: TObject);
begin
  case VerManListBox.ItemIndex of
    0: TVersionManagementAccess.TypedInstance(TGitAccess);
    1: TVersionManagementAccess.TypedInstance(TSubversionAccess);
  end;
end;

procedure TSingletonTestForm1.LogFromSingleton(const Msg: String);
begin
  if Pos('Creating', Msg) > 0 then
    CreationLabel.Caption := Msg
  else if Pos('Destroying', Msg) > 0 then
    DestructionLabel.Caption := Msg
  else
    SingletonListLabel.Caption := Msg;
end;

procedure TSingletonTestForm1.FormCreate(Sender: TObject);
begin
  LogHandler := @LogFromSingleton;
end;

initialization
  {$I SingletonTestFrm1.lrs}

end.

