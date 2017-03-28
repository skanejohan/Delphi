object WinQuizGameForm: TWinQuizGameForm
  Left = 352
  Top = 215
  Caption = 'WinQuizGameForm'
  ClientHeight = 359
  ClientWidth = 400
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
    400
    359)
  PixelsPerInch = 96
  TextHeight = 13
  object QuizesListBox: TListBox
    Left = 8
    Top = 64
    Width = 272
    Height = 80
    ItemHeight = 13
    TabOrder = 0
  end
  object StartQuizButton: TButton
    Left = 287
    Top = 7
    Width = 107
    Height = 25
    Caption = 'Start Quiz'
    TabOrder = 1
    OnClick = StartQuizButtonClick
  end
  object QuizPanel: TPanel
    Left = 8
    Top = 160
    Width = 386
    Height = 194
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Visible = False
    DesignSize = (
      386
      194)
    object QuestionNumberLabel: TLabel
      Left = 16
      Top = 8
      Width = 105
      Height = 13
      Caption = 'QuestionNumberLabel'
      Color = clBtnFace
      ParentColor = False
    end
    object QuestionLabel: TLabel
      Left = 16
      Top = 32
      Width = 68
      Height = 13
      Caption = 'QuestionLabel'
      Color = clBtnFace
      ParentColor = False
    end
    object Alt1Button: TButton
      Tag = 1
      Left = 16
      Top = 56
      Width = 363
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Alt1Button'
      TabOrder = 0
      OnClick = AltButtonClick
    end
    object Alt2Button: TButton
      Tag = 2
      Left = 16
      Top = 88
      Width = 363
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Alt2Button'
      TabOrder = 1
      OnClick = AltButtonClick
    end
    object Alt3Button: TButton
      Tag = 3
      Left = 16
      Top = 120
      Width = 363
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Alt3Button'
      TabOrder = 2
      OnClick = AltButtonClick
    end
    object Alt4Button: TButton
      Tag = 4
      Left = 16
      Top = 152
      Width = 363
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Alt4Button'
      TabOrder = 3
      OnClick = AltButtonClick
    end
  end
  object ResultPanel: TPanel
    Left = 8
    Top = 160
    Width = 386
    Height = 194
    TabOrder = 3
    Visible = False
  end
  object HardcodedRadioButton: TRadioButton
    Left = 8
    Top = 8
    Width = 179
    Height = 19
    Caption = 'Use hard-coded quiz manager'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = RadioButtonChange
  end
  object IniFileRadioButton: TRadioButton
    Left = 8
    Top = 36
    Width = 149
    Height = 19
    Caption = 'Use ini file quiz manager'
    TabOrder = 5
    OnClick = RadioButtonChange
  end
end
