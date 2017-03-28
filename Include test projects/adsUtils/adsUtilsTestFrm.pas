unit adsUtilsTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    CreateStepsGroupBox: TGroupBox;
    StartLabeledEdit: TLabeledEdit;
    StopLabeledEdit: TLabeledEdit;
    StepsLabeledEdit: TLabeledEdit;
    CreateStepsMemo: TMemo;
    TestCreateStepsButton: TButton;
    procedure TestCreateStepsButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses
  adsContainers,
  adsUtils;

{$R *.dfm}

procedure TForm1.TestCreateStepsButtonClick(Sender: TObject);
var
  i: Integer;
  List: TadsIntegerList;
begin
  List := TadsIntegerList.Create;
  try
    CreateStepsMemo.Clear;
    TadsUtils.CreateSteps(StrToInt(StartLabeledEdit.Text),
      StrToInt(StopLabeledEdit.Text),
      StrToInt(StepsLabeledEdit.Text), List);
    for i := 0 to Pred(List.Count) do
      CreateStepsMemo.Lines.Add(IntToStr(List[i]));
  finally
    List.Free;
  end;
end;

end.
