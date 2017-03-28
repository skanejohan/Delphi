unit adsWindowsEnumeratorClassesTestFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, adsWindowsEnumeratorClasses, ExtCtrls;

type
  TadsWindowsEnumeratorClassesTestForm = class(TForm)
    WindowsListView: TListView;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  protected
    Enumerator: TadsWindowsEnumerator;
  end{class};

var
  adsWindowsEnumeratorClassesTestForm: TadsWindowsEnumeratorClassesTestForm;

implementation

{$R *.dfm}

procedure TadsWindowsEnumeratorClassesTestForm.TimerTimer(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
begin
  Enumerator.Enumerate;
  WindowsListView.Clear;
  for i := 0 to Enumerator.WindowCount-1 do
    with Enumerator.Windows[i] do
    begin
      Item := WindowsListView.Items.Add;
      Item.Caption := IntToStr(Handle);
      Item.SubItems.Add(Caption);
      Item.SubItems.Add(Format('(%d,%d)-(%d,%d)', [Rect.Left, Rect.Top,
        Rect.Right, Rect.Bottom]));
    end{with};
end{procedure};

procedure TadsWindowsEnumeratorClassesTestForm.FormCreate(Sender: TObject);
begin
  Enumerator := TadsWindowsEnumerator.Create;
end{procedure};

procedure TadsWindowsEnumeratorClassesTestForm.FormDestroy(Sender: TObject);
begin
  Enumerator.Free;
end{procedure};

end.
