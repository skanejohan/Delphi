unit adsHeightMapGeneratorTestFrm;

{ © 2010 Aston Design Studio }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, adsHeightMapClass, ComCtrls;

type
  TadsHeightMapGeneratorTestForm = class(TForm)
    ElevationImage: TImage;
    PopulationImage: TImage;
    GenerateElevationButton: TButton;
    ElevationAndPopulationImageImage: TImage;
    ElevationLabel: TLabel;
    GeneratePopulationButton: TButton;
    PopulationLabel: TLabel;
    BreakpointTrackBar: TTrackBar;
    BreakpointValueLabel: TLabel;
    RoughnessTrackBar: TTrackBar;
    RoughnessLabel: TLabel;
    RoughnessValueLabel: TLabel;
    RegenerateButton: TButton;
    BreakpointLabel: TLabel;
    NoOfPopCentersTrackBar: TTrackBar;
    NoOfPopCentersLabel: TLabel;
    NoOfPopCentersValueLabel: TLabel;
    PopCenterHeightLabel: TLabel;
    PopCenterHeightTrackBar: TTrackBar;
    PopCenterHeightValueLabel: TLabel;
    PopCenterRadiusLabel: TLabel;
    PopCenterRadiusTrackBar: TTrackBar;
    PopCenterRadiusValueLabel: TLabel;
    procedure GenerateElevationButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GeneratePopulationButtonClick(Sender: TObject);
    procedure BreakpointTrackBarChange(Sender: TObject);
    procedure RegenerateButtonClick(Sender: TObject);
    procedure NoOfPopCentersTrackBarChange(Sender: TObject);
    procedure RoughnessTrackBarChange(Sender: TObject);
    procedure PopCenterHeightTrackBarChange(Sender: TObject);
    procedure PopCenterRadiusTrackBarChange(Sender: TObject);
  protected
    Elevation: THeightMap;
    Population: THeightMap;
    procedure DisplayElevation;
    procedure DisplayPopulation;
    procedure DisplayElevationAndPopulation;
  end{class};

var
  adsHeightMapGeneratorTestForm: TadsHeightMapGeneratorTestForm;

implementation

uses
  adsLoggerClass, adsHeightMapGeneratorClasses;

{$R *.dfm}

const
  Side = 256;

procedure TadsHeightMapGeneratorTestForm.DisplayElevation;
var
  X, Y: Integer;
begin
  with ElevationImage.Picture.Bitmap.Canvas do
    for X := 0 to Side do
      for Y := 0 to Side do
      begin
        Brush.Color := TColor(RGB(Elevation[X, Y], Elevation[X, Y],
          Elevation[X, Y]));
        FillRect(Rect(X, Y, X+1, Y+1));
      end{for};
end{procedure};

procedure TadsHeightMapGeneratorTestForm.DisplayPopulation;
var
  X, Y: Integer;
begin
  with PopulationImage.Picture.Bitmap.Canvas do
    for X := 0 to Side do
      for Y := 0 to Side do
      begin
        Brush.Color := TColor(RGB(Population[X, Y], Population[X, Y],
          Population[X, Y]));
        FillRect(Rect(X, Y, X+1, Y+1));
      end{for};
end{procedure};

procedure TadsHeightMapGeneratorTestForm.DisplayElevationAndPopulation;
var
  X, Y, BreakPoint: Integer;
begin
  BreakPoint := Elevation.Breakpoint[BreakpointTrackBar.Position];
  with ElevationAndPopulationImageImage.Picture.Bitmap.Canvas do
    for X := 0 to Side do
      for Y := 0 to Side do
      begin
        if Elevation[X, Y] > BreakPoint then
        begin
          if Population[X, Y] > 0 then
            Brush.Color := TColor(RGB(
              Population[X, Y] div 2,
              128-Population[X, Y] div 2,
              Population[X, Y] div 2))
          else
            Brush.Color := clGreen;
        end
        else
          Brush.Color := clBlue;
        FillRect(Rect(X, Y, X+1, Y+1));
      end{for};
  RegenerateButton.Enabled := False;
end{procedure};

procedure TadsHeightMapGeneratorTestForm.GenerateElevationButtonClick(Sender: TObject);
var
  ElevationGenerator: TMidpointDisplacementHeightMapGenerator;
begin
  Screen.Cursor := crHourglass;
  ElevationGenerator := TMidpointDisplacementHeightMapGenerator.Create;
  try
    Randomize;
    ElevationGenerator.MaxHeight := 255;
    ElevationGenerator.Roughness := RoughnessTrackBar.Position / 100;
    ElevationGenerator.Generate(Elevation);
    DisplayElevation;
    DisplayElevationAndPopulation;
  finally
    ElevationGenerator.Free;
    Screen.Cursor := crDefault;
  end{finally};
end{procedure};

procedure TadsHeightMapGeneratorTestForm.GeneratePopulationButtonClick(
  Sender: TObject);
var
  i: Integer;
  PopulationGenerator: TCircularPeakHeightMapGenerator;
begin
  Screen.Cursor := crHourglass;
  PopulationGenerator := TCircularPeakHeightMapGenerator.Create;
  try
    PopulationGenerator.MaxHeight := 255;
    for i := 0 to NoOfPopCentersTrackBar.Position-1 do
      PopulationGenerator.AddPeak(Random(256), Random(256),
        PopCenterHeightTrackBar.Position, PopCenterRadiusTrackBar.Position);
    PopulationGenerator.Generate(Population);
    DisplayPopulation;
    DisplayElevationAndPopulation;
  finally
    PopulationGenerator.Free;
    Screen.Cursor := crDefault;
  end{finally};
end{procedure};

procedure TadsHeightMapGeneratorTestForm.RegenerateButtonClick(
  Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    DisplayElevationAndPopulation;
  finally
    Screen.Cursor := crDefault;
  end{finally};
end{procedure};

procedure TadsHeightMapGeneratorTestForm.RoughnessTrackBarChange(
  Sender: TObject);
begin
  RoughnessValueLabel.Caption := IntToStr(RoughnessTrackBar.Position);
end{procedure};

procedure TadsHeightMapGeneratorTestForm.NoOfPopCentersTrackBarChange(
  Sender: TObject);
begin
  NoOfPopCentersValueLabel.Caption := IntToStr(NoOfPopCentersTrackBar.Position);
end{procedure};

procedure TadsHeightMapGeneratorTestForm.PopCenterHeightTrackBarChange(
  Sender: TObject);
begin
  PopCenterHeightValueLabel.Caption := IntToStr(PopCenterHeightTrackBar.Position);
end{procedure};

procedure TadsHeightMapGeneratorTestForm.PopCenterRadiusTrackBarChange(
  Sender: TObject);
begin
  PopCenterRadiusValueLabel.Caption := IntToStr(PopCenterRadiusTrackBar.Position);
end{procedure};

procedure TadsHeightMapGeneratorTestForm.BreakpointTrackBarChange(Sender: TObject);
begin
  BreakpointValueLabel.Caption := IntToStr(BreakpointTrackBar.Position);
  RegenerateButton.Enabled := True;
end{procedure};

procedure TadsHeightMapGeneratorTestForm.FormCreate(Sender: TObject);
begin
  Logger.AddSubscription(ChangeFileExt(Application.ExeName, '.log'));
  Elevation := THeightMap.Create;
  Population := THeightMap.Create;
  ElevationImage.Picture.Bitmap := TBitmap.Create;
  ElevationImage.Picture.Bitmap.Width := Side;
  ElevationImage.Picture.Bitmap.Height := Side;
  PopulationImage.Picture.Bitmap := TBitmap.Create;
  PopulationImage.Picture.Bitmap.Width := Side;
  PopulationImage.Picture.Bitmap.Height := Side;
  ElevationAndPopulationImageImage.Picture.Bitmap := TBitmap.Create;
  ElevationAndPopulationImageImage.Picture.Bitmap.Width := Side;
  ElevationAndPopulationImageImage.Picture.Bitmap.Height := Side;
end{procedure};

end.
