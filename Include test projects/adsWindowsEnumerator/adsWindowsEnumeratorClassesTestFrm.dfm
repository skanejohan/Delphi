object adsWindowsEnumeratorClassesTestForm: TadsWindowsEnumeratorClassesTestForm
  Left = 460
  Top = 295
  Width = 664
  Height = 446
  Caption = 'adsWindowsEnumeratorClassesTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object WindowsListView: TListView
    Left = 0
    Top = 0
    Width = 656
    Height = 412
    Align = alClient
    Columns = <
      item
        Caption = 'Handle'
        Width = 100
      end
      item
        Caption = 'Caption'
        Width = 400
      end
      item
        Caption = 'Rect'
        Width = 150
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 48
    Top = 24
  end
end
