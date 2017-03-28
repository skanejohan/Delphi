object adsTextEditDialog: TadsTextEditDialog
  Left = 234
  Top = 230
  BorderStyle = bsDialog
  Caption = 'Enter name'
  ClientHeight = 71
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object CancelBtn: TBitBtn
    Left = 168
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    NumGlyphs = 2
  end
  object OKBtn: TBitBtn
    Left = 88
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
    NumGlyphs = 2
  end
  object TextEdit: TEdit
    Left = 48
    Top = 12
    Width = 193
    Height = 21
    TabOrder = 0
  end
end
