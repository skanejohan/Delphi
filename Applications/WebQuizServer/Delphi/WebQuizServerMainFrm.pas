unit WebQuizServerMainFrm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, WebQuizServerManagerClass;

type
  TWebQuizServerMainForm = class(TForm)
    LogMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    Manager: TWebQuizServerManager;
    procedure HandleLog(const Msg: String);
  end;

var
  WebQuizServerMainForm: TWebQuizServerMainForm;

implementation

{$R *.dfm}

procedure TWebQuizServerMainForm.HandleLog(const Msg: String);
begin
  LogMemo.Lines.Add(Msg);
  LogMemo.Lines.SaveToFile(ParamStr(0) + '.log');
end;

procedure TWebQuizServerMainForm.FormCreate(Sender: TObject);
begin
  Manager := TWebQuizServerManager.Create;
  Manager.OnLog := HandleLog;
end;

end.

