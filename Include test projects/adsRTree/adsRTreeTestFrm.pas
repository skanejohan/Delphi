unit adsRTreeTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, adsRTree, StdCtrls, ExtCtrls;

type
  TForm11 = class(TForm)
    TreeMemo: TMemo;
    Splitter1: TSplitter;
    LogMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  protected
    Tree: TadsRTree;
    procedure TreeLog(Sender: TObject; const Msg: String);
    procedure InsertEntry(Left, Top, Right, Bottom, ID: Integer);
  end;

var
  Form11: TForm11;

implementation

{$R *.dfm}

uses
  adsGeometry;

//todo Insert (50,50)-(60,60), ID=5 ger fel träd!

procedure TForm11.InsertEntry(Left, Top, Right, Bottom, ID: Integer);
var
  SL: TStrings;
begin
  SL := TStringList.Create;
  TreeMemo.Lines.Add(Format('Insert (%d,%d)-(%d,%d), ID=%d', [Left, Top, Right, Bottom, ID]));
  Tree.Insert(adsRect(Left, Top, Right, Bottom), ID);
  Tree.Dump(SL);
  SL.Add('---------------------------------------------------');
  TreeMemo.Lines.AddStrings(SL);
  SL.Free;
end{procedure};

procedure TForm11.TreeLog(Sender: TObject; const Msg: String);
begin
  LogMemo.Lines.Add(Msg);
end{procedure};

procedure TForm11.FormCreate(Sender: TObject);
begin
  Tree := TadsRTree.Create(2, 3, TreeLog);
  InsertEntry(0, 0, 10, 10, 1);
  InsertEntry(10, 0, 20, 10, 2);
  InsertEntry(20, 0, 30, 10, 3);
  InsertEntry(0, 10, 10, 20, 4);
  InsertEntry(10, 10, 20, 20, 5);
  InsertEntry(20, 10, 30, 20, 6);
  InsertEntry(0, 20, 10, 30, 7);
  InsertEntry(10, 20, 20, 30, 8);
  InsertEntry(20, 20, 30, 30, 9);

  InsertEntry(10, 10, 20, 20, 1);
  InsertEntry(20, 20, 30, 30, 2);
  InsertEntry(30, 30, 40, 40, 3);
  InsertEntry(40, 40, 50, 50, 4);
  InsertEntry(50, 50, 60, 60, 5);
  InsertEntry(60, 60, 70, 70, 6);
  InsertEntry(70, 70, 80, 80, 7);
  InsertEntry(80, 80, 90, 90, 8);
end{procedure};

end.
