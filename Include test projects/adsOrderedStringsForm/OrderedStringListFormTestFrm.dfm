object OrderedStringListFormTestForm: TOrderedStringListFormTestForm
  Left = 138
  Top = 158
  Width = 547
  Height = 490
  Caption = 'OrderedStringListFormTestForm'
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
  object FileListBox1: TFileListBox
    Left = 256
    Top = 8
    Width = 273
    Height = 409
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = FileListBox1DblClick
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 8
    Top = 8
    Width = 241
    Height = 409
    FileList = FileListBox1
    ItemHeight = 16
    TabOrder = 1
  end
  object Button1: TButton
    Left = 456
    Top = 424
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
  end
end
