object adsDialogMethodsTestForm: TadsDialogMethodsTestForm
  Left = 192
  Top = 114
  Width = 430
  Height = 348
  Caption = 'adsDialogMethodsTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ShowMessagesButton: TButton
    Left = 144
    Top = 16
    Width = 137
    Height = 25
    Caption = 'Show messages'
    TabOrder = 0
    OnClick = ShowMessagesButtonClick
  end
  object InformationDialogButton: TButton
    Left = 144
    Top = 48
    Width = 137
    Height = 25
    Caption = 'Information dialog'
    TabOrder = 1
    OnClick = InformationDialogButtonClick
  end
  object ConfirmationDialogButton: TButton
    Left = 144
    Top = 80
    Width = 137
    Height = 25
    Caption = 'Confirmation dialog'
    TabOrder = 2
    OnClick = ConfirmationDialogButtonClick
  end
end
