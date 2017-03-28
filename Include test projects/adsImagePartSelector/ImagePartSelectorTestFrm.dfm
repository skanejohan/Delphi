object Form1: TForm1
  Left = 449
  Top = 223
  Width = 506
  Height = 431
  Caption = 'Form1'
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
  object ZoomInButton: TButton
    Left = 56
    Top = 144
    Width = 25
    Height = 25
    Caption = '+'
    TabOrder = 0
    OnClick = ZoomInButtonClick
  end
  object ZoomOutButton: TButton
    Left = 56
    Top = 168
    Width = 25
    Height = 25
    Caption = '-'
    TabOrder = 1
    OnClick = ZoomOutButtonClick
  end
  object PanUpButton: TButton
    Left = 56
    Top = 104
    Width = 25
    Height = 25
    Caption = 'U'
    TabOrder = 2
    OnClick = PanUpButtonClick
  end
  object PanLeftButton: TButton
    Left = 8
    Top = 156
    Width = 25
    Height = 25
    Caption = 'L'
    TabOrder = 3
    OnClick = PanLeftButtonClick
  end
  object PanRightButton: TButton
    Left = 104
    Top = 156
    Width = 25
    Height = 25
    Caption = 'R'
    TabOrder = 4
    OnClick = PanRightButtonClick
  end
  object PanDownButton: TButton
    Left = 56
    Top = 208
    Width = 25
    Height = 25
    Caption = 'D'
    TabOrder = 5
    OnClick = PanDownButtonClick
  end
  object DisplayButton: TButton
    Left = 32
    Top = 18
    Width = 75
    Height = 25
    Caption = 'Display'
    TabOrder = 6
    OnClick = DisplayButtonClick
  end
  object Panel1: TPanel
    Left = 144
    Top = 8
    Width = 329
    Height = 393
    TabOrder = 7
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 327
      Height = 391
      Align = alClient
    end
  end
  object CheckBox1: TCheckBox
    Left = 32
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Maximized'
    TabOrder = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 112
    Top = 16
  end
end
