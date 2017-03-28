unit adsExecutorClassTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TadsExecutorClassTestForm = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    FileName: String;
    procedure ExecutorIdle;
  end{class};

var
  adsExecutorClassTestForm: TadsExecutorClassTestForm;

implementation

uses
  adsExecutorClass;

{$R *.dfm}

procedure TadsExecutorClassTestForm.Button1Click(Sender: TObject);
var
  FC: TStrings;
begin
  FC := TStringList.Create;
  try
    FC.Add('echo %1');
    FC.Add('echo %2');
    FC.Add('echo %3');
    FC.Add('pause');
    FC.SaveToFile(FileName);
    Label1.Caption := 'executing';
    Button1.Enabled := False;
    TadsExecutor.Execute(FileName + ' one two three', CheckBox1.Checked, '',
      ExecutorIdle);
  finally
    Label1.Caption := 'done';
    Button1.Enabled := True;
    FC.Free;
  end{finally};
end{procedure};

procedure TadsExecutorClassTestForm.ExecutorIdle;
begin
  Application.ProcessMessages;
end;

procedure TadsExecutorClassTestForm.FormCreate(Sender: TObject);
begin
  FileName := GetCurrentDir + '\bat.bat';
end;

procedure TadsExecutorClassTestForm.FormDestroy(Sender: TObject);
begin
  DeleteFile(FileName);
end;

end.
