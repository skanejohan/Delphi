object Form1: TForm1
  Left = 151
  Top = 300
  Width = 940
  Height = 412
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
  object DirectoryListBox1: TDirectoryListBox
    Left = 8
    Top = 8
    Width = 209
    Height = 337
    ItemHeight = 16
    TabOrder = 0
  end
  object Button1: TButton
    Left = 224
    Top = 40
    Width = 25
    Height = 25
    Caption = '>>'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 352
    Width = 377
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Button3: TButton
    Left = 224
    Top = 8
    Width = 25
    Height = 25
    Caption = '>'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 224
    Top = 72
    Width = 25
    Height = 25
    Caption = '<'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 392
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Set'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button2: TButton
    Left = 472
    Top = 8
    Width = 25
    Height = 25
    Caption = '>'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button6: TButton
    Left = 472
    Top = 40
    Width = 25
    Height = 25
    Caption = '<'
    TabOrder = 8
    OnClick = Button6Click
  end
  object AllowedDirectoriesMemo: TMemo
    Left = 504
    Top = 8
    Width = 425
    Height = 337
    TabOrder = 7
  end
end
