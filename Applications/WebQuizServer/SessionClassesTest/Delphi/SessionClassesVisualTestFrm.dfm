object SessionClassesVisualTestForm: TSessionClassesVisualTestForm
  Left = 70
  Top = 118
  Caption = 'SessionClassesVisualTestForm'
  ClientHeight = 513
  ClientWidth = 1239
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1239
    513)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 991
    Top = 478
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'New'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 1073
    Top = 478
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Get'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 1156
    Top = 478
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Remove'
    TabOrder = 2
    OnClick = Button3Click
  end
  object HashTableMemo: TMemo
    Left = 10
    Top = 10
    Width = 1221
    Height = 432
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object IDEdit: TEdit
    Left = 906
    Top = 478
    Width = 80
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 4
  end
  object SessionTimeListEdit: TEdit
    Left = 10
    Top = 448
    Width = 1216
    Height = 21
    ReadOnly = True
    TabOrder = 5
  end
  object StatisticsButton: TButton
    Left = 9
    Top = 478
    Width = 75
    Height = 25
    Caption = 'Statistics'
    TabOrder = 6
    OnClick = StatisticsButtonClick
  end
end
