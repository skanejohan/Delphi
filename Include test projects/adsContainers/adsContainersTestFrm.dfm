object adsContainersTestForm: TadsContainersTestForm
  Left = 271
  Top = 123
  Width = 456
  Height = 444
  Caption = 'adsContainersTestForm'
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
  object ResultMemo: TMemo
    Left = 8
    Top = 40
    Width = 433
    Height = 361
    TabOrder = 0
  end
  object IntegerListButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Integer List'
    TabOrder = 1
    OnClick = IntegerListButtonClick
  end
end
