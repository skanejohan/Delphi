object adsOrderedStringsForm: TadsOrderedStringsForm
  Left = 376
  Top = 209
  Width = 549
  Height = 388
  Caption = 'Ordered Strings'
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
  object StringsListBox: TListBox
    Left = 0
    Top = 0
    Width = 541
    Height = 354
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnClick = StringsListBoxClick
  end
  object PopupMenu1: TPopupMenu
    Left = 24
    Top = 16
    object Moveup1: TMenuItem
      Caption = 'Move &up'
      OnClick = Moveup1Click
    end
    object Movedown1: TMenuItem
      Caption = 'Move &down'
      OnClick = Movedown1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Deleteselecteditems1: TMenuItem
      Caption = 'Delete selected item(s)'
      OnClick = Deleteselecteditems1Click
    end
    object Clearall1: TMenuItem
      Caption = 'Clear all'
      OnClick = Clearall1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Save1: TMenuItem
      Caption = '&Save'
      OnClick = Save1Click
    end
    object Load1: TMenuItem
      Caption = '&Load'
      OnClick = Load1Click
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Text files|*.txt'
    Left = 80
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files|*.txt'
    Left = 128
    Top = 16
  end
end
