unit ImageMethodsTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtDlgs, StdCtrls, adsImageMethods, ExtCtrls, Jpeg;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure CompareUpdate(Current, Total: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  adsFileMethods;

procedure TForm1.Button1Click(Sender: TObject);
var
  FirstImage: String;
begin
  if OpenPictureDialog1.Execute then
  begin
    FirstImage := OpenPictureDialog1.FileName;
    if OpenPictureDialog1.Execute then
    begin
      if ImagesMayBeEqual(FirstImage, OpenPictureDialog1.FileName) then
        ShowMessage('Images may be equal')
      else
        ShowMessage('Images differ');
    end;
  end;
end;

procedure TForm1.CompareUpdate(Current, Total: Integer);
begin
  Button2.Caption := IntToStr(Current) + ' of ' + IntToStr(Total);
  Button2.Update;
end; {procedure}

procedure TForm1.Button2Click(Sender: TObject);
var
  Dir: String;
  Files, ComparedFiles: TStrings;
begin
  Files := TStringList.Create;
  ComparedFiles := TStringList.Create;
  try
    if OpenPictureDialog1.Execute then
    begin
      Dir := ExtractFileDir(OpenPictureDialog1.FileName);
      GetFiles(Dir, Files, 'jpg');
      CompareImages(Files, Memo1.Lines, CompareUpdate);
    end;
  finally
    Files.Free;
    ComparedFiles.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenPictureDialog1.InitialDir := GetCurrentDir;
end;

end.
