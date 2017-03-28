object adsMapControlTestForm: TadsMapControlTestForm
  Left = 207
  Top = 163
  Width = 810
  Height = 641
  Caption = 'adsMapControlTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 552
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 40
    Top = 576
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object CheckBox1: TCheckBox
    Left = 40
    Top = 552
    Width = 97
    Height = 17
    Caption = 'Threaded'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckBox1Click
  end
end
