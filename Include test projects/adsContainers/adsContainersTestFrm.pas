unit adsContainersTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, adsContainers, StdCtrls;

type
  TadsContainersTestForm = class(TForm)
    ResultMemo: TMemo;
    IntegerListButton: TButton;
    procedure IntegerListButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    IntegerList: TadsIntegerList;
  end; {class}

var
  adsContainersTestForm: TadsContainersTestForm;

implementation

{$R *.dfm}

procedure TadsContainersTestForm.IntegerListButtonClick(Sender: TObject);

  function IntegerListIsSorted: Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to IntegerList.Count-2 do
      if IntegerList[i] > IntegerList[i+1] then
      begin
        Result := False;
        Break;
      end; {if}
  end; {function}

  procedure TestIntegerList(Elements: Integer);
  var
    i: Integer;
    TicksBefore, TicksAfter: Cardinal;
  begin
    {Add elements unsorted}
    TicksBefore := GetTickCount;
    for i := 0 to Pred(Elements) do IntegerList.Add(Random(100000));
    TicksAfter := GetTickCount;
    ResultMemo.Lines.Add(Format('%d integers added in %d milliseconds',
      [Elements, TicksAfter-TicksBefore]));
    if IntegerListIsSorted then ResultMemo.Lines.Add('ERROR: LIST IS SORTED');

    {Sort the elements}
    TicksBefore := GetTickCount;
    IntegerList.Sort;
    TicksAfter := GetTickCount;
    ResultMemo.Lines.Add(Format('%d integers sorted in %d milliseconds',
      [Elements, TicksAfter-TicksBefore]));
    if not IntegerListIsSorted then ResultMemo.Lines.Add('ERROR: LIST IS NOT SORTED');

    {Clear the list}
    TicksBefore := GetTickCount;
    IntegerList.Clear;
    TicksAfter := GetTickCount;
    ResultMemo.Lines.Add(Format('%d integers, list cleared in %d milliseconds',
      [Elements, TicksAfter-TicksBefore]));

    if Elements > 100000 then
      ResultMemo.Lines.Add(Format('%d integers, sorted adding not tested - ' +
        'use unsorted adding, then sort', [Elements]))
    else
    begin

      {Add elements sorted}
      TicksBefore := GetTickCount;
      for i := 0 to Pred(Elements) do IntegerList.AddSorted(Random(100000));
      TicksAfter := GetTickCount;
      ResultMemo.Lines.Add(Format('%d integers added (sorted) in %d milliseconds',
        [Elements, TicksAfter-TicksBefore]));
      if not IntegerListIsSorted then ResultMemo.Lines.Add('ERROR: LIST IS NOT SORTED');

      {Clear the list}
      TicksBefore := GetTickCount;
      IntegerList.Clear;
      TicksAfter := GetTickCount;
      ResultMemo.Lines.Add(Format('%d integers, list cleared in %d milliseconds',
        [Elements, TicksAfter-TicksBefore]));
    end; {else}
  end; {procedure}

begin
  TestIntegerList(1000);
  TestIntegerList(10000);
  TestIntegerList(100000);
  TestIntegerList(1000000);
  TestIntegerList(10000000);
end; {procedure}

procedure TadsContainersTestForm.FormCreate(Sender: TObject);
begin
  IntegerList := TadsIntegerList.Create;
end; {procedure}

procedure TadsContainersTestForm.FormDestroy(Sender: TObject);
begin
  IntegerList.Free;
end; {procedure}

end.
