object AStarTestForm: TAStarTestForm
  Left = 497
  Top = 220
  Width = 454
  Height = 499
  Caption = 'AStarTestForm'
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
  object MapGrid: TStringGrid
    Left = 8
    Top = 32
    Width = 425
    Height = 425
    ColCount = 20
    DefaultColWidth = 20
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 20
    FixedRows = 0
    TabOrder = 0
    OnDrawCell = MapGridDrawCell
    OnSelectCell = MapGridSelectCell
  end
  object GridTypeComboBox: TComboBox
    Left = 8
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = 'Empty grid'
    OnChange = GridTypeComboBoxChange
    Items.Strings = (
      'Empty grid'
      'Grid with blocks'
      'Labyrinth'
      'Empty grid with marshland')
  end
end
