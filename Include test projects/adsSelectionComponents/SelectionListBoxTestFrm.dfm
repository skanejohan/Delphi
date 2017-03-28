object Form1: TForm1
  Left = 191
  Top = 54
  Width = 865
  Height = 670
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
  object Label1: TLabel
    Left = 8
    Top = 76
    Width = 45
    Height = 13
    Caption = 'Selected:'
  end
  object Label3: TLabel
    Left = 8
    Top = 140
    Width = 44
    Height = 13
    Caption = 'First item:'
  end
  object Label2: TLabel
    Left = 8
    Top = 404
    Width = 45
    Height = 13
    Caption = 'Selected:'
  end
  object SelectedEdit: TEdit
    Left = 64
    Top = 72
    Width = 185
    Height = 21
    TabOrder = 0
    Text = 'SelectedEdit'
  end
  object WriteSelectedButton: TButton
    Left = 192
    Top = 96
    Width = 59
    Height = 25
    Caption = 'Write'
    TabOrder = 1
    OnClick = WriteSelectedButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 184
    Top = 40
    Width = 65
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Sorted'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object FirstItemEdit: TEdit
    Left = 80
    Top = 136
    Width = 169
    Height = 21
    TabOrder = 3
    Text = 'All countries'
  end
  object SetFirstItemButton: TButton
    Left = 192
    Top = 160
    Width = 59
    Height = 25
    Caption = 'Set'
    TabOrder = 4
    OnClick = SetFirstItemButtonClick
  end
  object ClearFirstItemCheckBox: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Clear FirstItem'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object ClearEditCheckBox: TCheckBox
    Left = 8
    Top = 32
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Clear Edit'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object ClearButton: TButton
    Left = 112
    Top = 8
    Width = 59
    Height = 25
    Caption = 'Clear'
    TabOrder = 7
    OnClick = ClearButtonClick
  end
  object AddItemsButton: TButton
    Left = 192
    Top = 8
    Width = 59
    Height = 25
    Caption = 'Add Items'
    TabOrder = 8
    OnClick = AddItemsButtonClick
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 192
    Width = 241
    Height = 105
    Caption = 'Edit mode'
    ItemIndex = 0
    Items.Strings = (
      'emStatic'
      'emAllowFreeText'
      'emAllowFreeTextUpdating')
    TabOrder = 9
    OnClick = RadioGroup1Click
  end
  object Panel2: TPanel
    Left = 0
    Top = 312
    Width = 849
    Height = 9
    TabOrder = 10
  end
  object Edit1: TEdit
    Left = 64
    Top = 400
    Width = 185
    Height = 21
    TabOrder = 11
    Text = 'SelectedEdit'
  end
  object Button1: TButton
    Left = 192
    Top = 424
    Width = 59
    Height = 25
    Caption = 'Write'
    TabOrder = 12
    OnClick = Button1Click
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 340
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Clear Edit'
    Checked = True
    State = cbChecked
    TabOrder = 13
  end
  object Button3: TButton
    Left = 112
    Top = 336
    Width = 59
    Height = 25
    Caption = 'Clear'
    TabOrder = 14
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 192
    Top = 336
    Width = 59
    Height = 25
    Caption = 'Add Items'
    TabOrder = 15
    OnClick = Button4Click
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 456
    Width = 241
    Height = 105
    Caption = 'Edit mode'
    ItemIndex = 0
    Items.Strings = (
      'emStatic'
      'emAllowFreeText'
      'emAllowFreeTextUpdating')
    TabOrder = 16
    OnClick = RadioGroup2Click
  end
  object Memo1: TMemo
    Left = 464
    Top = 8
    Width = 377
    Height = 257
    TabOrder = 17
  end
  object GetListItemsButton: TButton
    Left = 768
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Get items'
    TabOrder = 18
    OnClick = GetListItemsButtonClick
  end
  object Memo2: TMemo
    Left = 464
    Top = 336
    Width = 377
    Height = 257
    TabOrder = 19
  end
  object GetTreeItemsButton: TButton
    Left = 768
    Top = 600
    Width = 75
    Height = 25
    Caption = 'Get items'
    TabOrder = 20
    OnClick = GetTreeItemsButtonClick
  end
end
