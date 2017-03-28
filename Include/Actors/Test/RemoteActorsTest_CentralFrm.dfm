object RemoteActorsTest_CentralForm: TRemoteActorsTest_CentralForm
  Left = 0
  Top = 0
  Caption = 'Remote Actors Test'
  ClientHeight = 340
  ClientWidth = 855
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 297
    Height = 321
    TabOrder = 0
  end
  object SpinEdit: TSpinEdit
    Left = 311
    Top = 8
    Width = 163
    Height = 22
    MaxValue = 9100
    MinValue = 9001
    TabOrder = 1
    Value = 9001
  end
  object ActivateButton: TButton
    Left = 311
    Top = 32
    Width = 163
    Height = 25
    Caption = 'Activate'
    TabOrder = 2
    OnClick = ActivateButtonClick
  end
  object CreateActorButton: TButton
    Left = 310
    Top = 63
    Width = 164
    Height = 25
    Caption = 'Create actor'
    TabOrder = 3
    OnClick = CreateActorButtonClick
  end
  object Button1: TButton
    Left = 310
    Top = 94
    Width = 164
    Height = 25
    Caption = 'Inc'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 310
    Top = 125
    Width = 164
    Height = 25
    Caption = 'Dec'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 310
    Top = 156
    Width = 164
    Height = 25
    Caption = 'Ask'
    TabOrder = 6
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 480
    Top = 8
    Width = 361
    Height = 313
    ItemHeight = 13
    TabOrder = 7
  end
end
