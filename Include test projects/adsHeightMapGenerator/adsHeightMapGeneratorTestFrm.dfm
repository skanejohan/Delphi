object adsHeightMapGeneratorTestForm: TadsHeightMapGeneratorTestForm
  Left = 327
  Top = 272
  Width = 854
  Height = 486
  Caption = 'adsHeightMapGeneratorTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ElevationImage: TImage
    Left = 8
    Top = 24
    Width = 256
    Height = 256
  end
  object PopulationImage: TImage
    Left = 294
    Top = 24
    Width = 256
    Height = 256
  end
  object ElevationAndPopulationImageImage: TImage
    Left = 580
    Top = 24
    Width = 256
    Height = 256
  end
  object ElevationLabel: TLabel
    Left = 8
    Top = 8
    Width = 211
    Height = 13
    Caption = 'Elevation (midpoint displacement generator)'
  end
  object PopulationLabel: TLabel
    Left = 296
    Top = 8
    Width = 172
    Height = 13
    Caption = 'Population (circular peak generator)'
  end
  object BreakpointValueLabel: TLabel
    Left = 816
    Top = 308
    Width = 12
    Height = 13
    Caption = '30'
  end
  object RoughnessLabel: TLabel
    Left = 16
    Top = 288
    Width = 75
    Height = 13
    Caption = 'Roughness (%)'
  end
  object RoughnessValueLabel: TLabel
    Left = 240
    Top = 308
    Width = 12
    Height = 13
    Caption = '20'
  end
  object BreakpointLabel: TLabel
    Left = 588
    Top = 288
    Width = 51
    Height = 13
    Caption = 'Breakpoint'
  end
  object NoOfPopCentersLabel: TLabel
    Left = 304
    Top = 288
    Width = 142
    Height = 13
    Caption = 'Number of population centers'
  end
  object NoOfPopCentersValueLabel: TLabel
    Left = 528
    Top = 308
    Width = 6
    Height = 13
    Caption = '3'
  end
  object PopCenterHeightLabel: TLabel
    Left = 304
    Top = 328
    Width = 31
    Height = 13
    Caption = 'Height'
  end
  object PopCenterHeightValueLabel: TLabel
    Left = 528
    Top = 348
    Width = 18
    Height = 13
    Caption = '100'
  end
  object PopCenterRadiusLabel: TLabel
    Left = 304
    Top = 368
    Width = 32
    Height = 13
    Caption = 'Radius'
  end
  object PopCenterRadiusValueLabel: TLabel
    Left = 528
    Top = 388
    Width = 12
    Height = 13
    Caption = '80'
  end
  object GenerateElevationButton: TButton
    Left = 128
    Top = 416
    Width = 126
    Height = 25
    Caption = 'Generate elevation'
    TabOrder = 0
    OnClick = GenerateElevationButtonClick
  end
  object GeneratePopulationButton: TButton
    Left = 408
    Top = 416
    Width = 129
    Height = 25
    Caption = 'Generate population'
    TabOrder = 1
    OnClick = GeneratePopulationButtonClick
  end
  object BreakpointTrackBar: TTrackBar
    Left = 588
    Top = 304
    Width = 217
    Height = 25
    Max = 100
    Position = 30
    TabOrder = 2
    TickStyle = tsNone
    OnChange = BreakpointTrackBarChange
  end
  object RoughnessTrackBar: TTrackBar
    Left = 16
    Top = 304
    Width = 217
    Height = 25
    Max = 100
    Position = 20
    TabOrder = 3
    TickStyle = tsNone
    OnChange = RoughnessTrackBarChange
  end
  object RegenerateButton: TButton
    Left = 696
    Top = 416
    Width = 131
    Height = 25
    Caption = 'Regenerate'
    TabOrder = 4
    OnClick = RegenerateButtonClick
  end
  object NoOfPopCentersTrackBar: TTrackBar
    Left = 304
    Top = 304
    Width = 217
    Height = 25
    Position = 3
    TabOrder = 5
    TickStyle = tsNone
    OnChange = NoOfPopCentersTrackBarChange
  end
  object PopCenterHeightTrackBar: TTrackBar
    Left = 304
    Top = 344
    Width = 217
    Height = 25
    Max = 100
    Position = 100
    TabOrder = 6
    TickStyle = tsNone
    OnChange = PopCenterHeightTrackBarChange
  end
  object PopCenterRadiusTrackBar: TTrackBar
    Left = 304
    Top = 384
    Width = 217
    Height = 25
    Max = 200
    Position = 80
    TabOrder = 7
    TickStyle = tsNone
    OnChange = PopCenterRadiusTrackBarChange
  end
end
