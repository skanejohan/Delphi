object CounterTestForm: TCounterTestForm
  Left = 0
  Top = 0
  Caption = 'CounterTestForm'
  ClientHeight = 337
  ClientWidth = 635
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
  object Label1: TLabel
    Left = 296
    Top = 144
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Button1: TButton
    Left = 528
    Top = 24
    Width = 75
    Height = 25
    Caption = 'INC'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 528
    Top = 56
    Width = 75
    Height = 25
    Caption = 'DEC'
    TabOrder = 1
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 40
    Top = 16
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 152
    Top = 72
  end
end
