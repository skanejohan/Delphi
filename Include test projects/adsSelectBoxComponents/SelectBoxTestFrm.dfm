object Form1: TForm1
  Left = 313
  Top = 262
  Width = 747
  Height = 448
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
  object SelectBoxPanel: TPanel
    Left = 8
    Top = 224
    Width = 201
    Height = 177
    BevelOuter = bvLowered
    TabOrder = 0
  end
  object DBSelectBoxPanel: TPanel
    Left = 216
    Top = 224
    Width = 185
    Height = 177
    BevelOuter = bvLowered
    TabOrder = 1
  end
  object ListBox1: TListBox
    Left = 8
    Top = 0
    Width = 201
    Height = 217
    ItemHeight = 13
    Items.Strings = (
      'Sweden;Blue;Yellow'
      'Denmark;Red;White'
      'Germany;Red;Yellow;Black'
      'Italy;Red;White;Green'
      'France;Red;White;Blue')
    MultiSelect = True
    TabOrder = 2
    OnClick = ListBox1Click
  end
  object ListBox2: TListBox
    Left = 216
    Top = 0
    Width = 185
    Height = 217
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 3
    OnClick = ListBox2Click
  end
  object Edit1: TEdit
    Left = 600
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'Germany'
  end
  object Button1: TButton
    Left = 648
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 4
    OnClick = Button1Click
  end
  object CategoryTable: TTable
    TableName = 'Categories.db'
    Left = 352
    Top = 344
  end
  object ElementTable: TTable
    TableName = 'Elements.db'
    Left = 312
    Top = 344
  end
  object ElementCategoryTable: TTable
    TableName = 'ElementCategories.db'
    Left = 272
    Top = 344
  end
  object CategoryDS: TDataSource
    DataSet = CategoryTable
    Left = 352
    Top = 304
  end
  object ElementDS: TDataSource
    DataSet = ElementTable
    Left = 312
    Top = 304
  end
  object CategoryElementDS: TDataSource
    DataSet = ElementCategoryTable
    Left = 272
    Top = 304
  end
end
