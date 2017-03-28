object adsExecutorClassTestForm: TadsExecutorClassTestForm
  Left = 192
  Top = 114
  Width = 356
  Height = 181
  Caption = 'adsExecutorClassTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 80
    Width = 37
    Height = 13
    Caption = 'inactive'
  end
  object Button1: TButton
    Left = 32
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 16
    Width = 233
    Height = 17
    Caption = 'Wait for execution to finish'
    TabOrder = 1
  end
end
