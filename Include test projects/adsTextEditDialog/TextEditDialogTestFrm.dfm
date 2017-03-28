object TextEditDialogTestForm: TTextEditDialogTestForm
  Left = 215
  Top = 125
  Width = 294
  Height = 92
  Caption = 'TextEditDialogTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Text:'
  end
  object Edit1: TEdit
    Left = 40
    Top = 4
    Width = 137
    Height = 21
    Enabled = False
    TabOrder = 0
    Text = 'A text'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 32
    Width = 145
    Height = 17
    Caption = 'Allow empty text field'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 192
    Top = 2
    Width = 75
    Height = 25
    Caption = 'Edit text'
    TabOrder = 2
    OnClick = Button1Click
  end
end
