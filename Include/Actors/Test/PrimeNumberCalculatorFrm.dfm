object PrimeNumberCalculatorForm: TPrimeNumberCalculatorForm
  Left = 0
  Top = 0
  Caption = 'Primes'
  ClientHeight = 337
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    202
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 185
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object Button1: TButton
    Left = 119
    Top = 304
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Calculate!'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ThreadsEdit: TLabeledEdit
    Left = 8
    Top = 308
    Width = 97
    Height = 21
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 43
    EditLabel.Height = 13
    EditLabel.Caption = 'Threads:'
    TabOrder = 2
    Text = '4'
  end
end
