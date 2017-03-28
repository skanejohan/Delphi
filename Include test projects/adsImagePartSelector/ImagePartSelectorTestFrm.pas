unit ImagePartSelectorTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  adsImagePartSelectorClass, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    ZoomInButton: TButton;
    ZoomOutButton: TButton;
    PanUpButton: TButton;
    PanLeftButton: TButton;
    PanRightButton: TButton;
    PanDownButton: TButton;
    OpenDialog1: TOpenDialog;
    DisplayButton: TButton;
    Panel1: TPanel;
    Image1: TImage;
    CheckBox1: TCheckBox;
    procedure DisplayButtonClick(Sender: TObject);
    procedure ZoomInButtonClick(Sender: TObject);
    procedure ZoomOutButtonClick(Sender: TObject);
    procedure PanUpButtonClick(Sender: TObject);
    procedure PanRightButtonClick(Sender: TObject);
    procedure PanDownButtonClick(Sender: TObject);
    procedure PanLeftButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    ImagePartSelector: TImagePartSelector;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DisplayButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ImagePartSelector.Load(OpenDialog1.FileName);
    ImagePartSelector.Draw(CheckBox1.Checked);
  end;
end;

procedure TForm1.ZoomInButtonClick(Sender: TObject);
begin
  ImagePartSelector.ZoomIn(20);
end;

procedure TForm1.ZoomOutButtonClick(Sender: TObject);
begin
  ImagePartSelector.ZoomOut(20);
end;

procedure TForm1.PanUpButtonClick(Sender: TObject);
begin
  ImagePartSelector.Pan(pdUp, 20);
end;

procedure TForm1.PanRightButtonClick(Sender: TObject);
begin
  ImagePartSelector.Pan(pdRight, 20);
end;

procedure TForm1.PanDownButtonClick(Sender: TObject);
begin
  ImagePartSelector.Pan(pdDown, 20);
end;

procedure TForm1.PanLeftButtonClick(Sender: TObject);
begin
  ImagePartSelector.Pan(pdLeft, 20);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImagePartSelector := TImagePartSelector.Create;
  ImagePartSelector.ClearColor := Self.Color;
  ImagePartSelector.Image := Image1;
  OpenDialog1.InitialDir := GetCurrentDir;
end;

end.
