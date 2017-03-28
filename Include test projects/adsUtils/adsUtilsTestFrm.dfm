object Form1: TForm1
  Left = 357
  Top = 334
  Width = 528
  Height = 345
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CreateStepsGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 185
    Caption = 'CreateSteps'
    TabOrder = 0
    object StartLabeledEdit: TLabeledEdit
      Left = 8
      Top = 40
      Width = 121
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Start'
      TabOrder = 0
    end
    object StopLabeledEdit: TLabeledEdit
      Left = 8
      Top = 80
      Width = 121
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Stop'
      TabOrder = 1
    end
    object StepsLabeledEdit: TLabeledEdit
      Left = 8
      Top = 120
      Width = 121
      Height = 21
      EditLabel.Width = 27
      EditLabel.Height = 13
      EditLabel.Caption = 'Steps'
      TabOrder = 2
    end
    object CreateStepsMemo: TMemo
      Left = 136
      Top = 40
      Width = 41
      Height = 137
      TabOrder = 3
    end
    object TestCreateStepsButton: TButton
      Left = 8
      Top = 152
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 4
      OnClick = TestCreateStepsButtonClick
    end
  end
end
