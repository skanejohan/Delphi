object Form1: TForm1
  Left = 268
  Top = 297
  Width = 665
  Height = 363
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    657
    329)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Compare'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 32
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Different'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 112
    Top = 8
    Width = 537
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object OpenPictureDialog1: TOpenPictureDialog
    InitialDir = 'C:\Data\Delphi\Include\Units\Projects\ImageMethods\Data'
    Left = 8
  end
end
