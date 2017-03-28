object adsThreadedDrawingControlTestForm: TadsThreadedDrawingControlTestForm
  Left = 205
  Top = 234
  Width = 627
  Height = 502
  Caption = 'adsThreadedDrawingControlTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    619
    468)
  PixelsPerInch = 96
  TextHeight = 13
  object UpdateIntervalLabel: TLabel
    Left = 360
    Top = 429
    Width = 103
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Update interval (MS):'
  end
  object DrawButton: TButton
    Left = 8
    Top = 424
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Draw'
    TabOrder = 0
    OnClick = DrawButtonClick
  end
  object StopButton: TButton
    Left = 127
    Top = 424
    Width = 114
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Stop'
    TabOrder = 1
    OnClick = StopButtonClick
  end
  object UpdateIntervalSpinEdit: TSpinEdit
    Left = 472
    Top = 426
    Width = 57
    Height = 22
    Anchors = [akRight, akBottom]
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object ApplyButton: TButton
    Left = 535
    Top = 424
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 3
    OnClick = ApplyButtonClick
  end
  object DrawModeComboBox: TComboBox
    Left = 248
    Top = 424
    Width = 105
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'Threaded'
    OnChange = DrawModeComboBoxChange
    Items.Strings = (
      'Threaded'
      'Threaded (timer)'
      'Unthreade')
  end
  object StartDrawingTimer: TTimer
    Interval = 5
    OnTimer = StartDrawingTimerTimer
    Left = 16
    Top = 24
  end
end
