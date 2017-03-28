unit SessionClassesVisualTestFrm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  SessionClasses, TestSessionClasses;

type
  TSessionClassesVisualTestForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    StatisticsButton: TButton;
    SessionTimeListEdit: TEdit;
    IDEdit: TEdit;
    HashTableMemo: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure StatisticsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    SessionManager: TTestSessionManager;
    procedure DrawSessionManager;
  end;

var
  SessionClassesVisualTestForm: TSessionClassesVisualTestForm;

implementation

{$R *.dfm}

procedure TSessionClassesVisualTestForm.FormCreate(Sender: TObject);
begin
  SessionManager := TTestSessionManager.Create(TSession);
  DrawSessionManager;
end;

procedure TSessionClassesVisualTestForm.FormDestroy(Sender: TObject);
begin
  SessionManager.Free;
end;

procedure TSessionClassesVisualTestForm.Button1Click(Sender: TObject);
begin
  SessionManager.RandomSessionIDs := False;
  IDEdit.Text := IntToStr(SessionManager.NewSession);
  DrawSessionManager;
  IDEdit.SelectAll;
  IDEdit.SetFocus;
end;

procedure TSessionClassesVisualTestForm.Button2Click(Sender: TObject);
begin
  SessionManager.GetSession(StrToIntDef(IDEdit.Text, 0));
  DrawSessionManager;
  IDEdit.SelectAll;
  IDEdit.SetFocus;
end;

procedure TSessionClassesVisualTestForm.Button3Click(Sender: TObject);
begin
  SessionManager.RemoveSession(StrToIntDef(IDEdit.Text, 0));
  DrawSessionManager;
  IDEdit.SelectAll;
  IDEdit.SetFocus;
end;

procedure TSessionClassesVisualTestForm.StatisticsButtonClick(Sender: TObject);
var
  Strings: TStrings;

  procedure CheckStatistics(NoOfSessions: Cardinal);
  var
    i: Cardinal;
  begin
    SessionManager.Clear;
    HashTableMemo.Lines.Add(Format('----- %d sessions -----', [NoOfSessions]));
    for i := 0 to NoOfSessions-1 do
      SessionManager.NewSession;
    SessionManager.DrawHashTableStatistics(Strings);
    HashTableMemo.Lines.AddStrings(Strings);
  end;

begin
  Strings := TStringList.Create;
  try
    HashTableMemo.Lines.Clear;
    SessionManager.RandomSessionIDs := True;
    CheckStatistics(10000);
    CheckStatistics(20000);
    CheckStatistics(40000);
    CheckStatistics(100000);
    CheckStatistics(500000);
    CheckStatistics(1000000);
    SessionManager.Clear;
  finally
    Strings.Free;
  end;
end;

procedure TSessionClassesVisualTestForm.DrawSessionManager;
begin
  SessionManager.DrawHashTableContents(HashTableMemo.Lines);
  SessionTimeListEdit.Text := SessionManager.DrawTimeList;
end;

end.

