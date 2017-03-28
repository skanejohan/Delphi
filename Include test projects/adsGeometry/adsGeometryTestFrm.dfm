object Form1: TForm1
  Left = 261
  Top = 283
  Width = 620
  Height = 339
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
  object SameSideImage: TImage
    Left = 8
    Top = 56
    Width = 185
    Height = 201
    OnMouseDown = SameSideImageMouseDown
  end
  object SameSideInfoLabel: TLabel
    Left = 8
    Top = 8
    Width = 183
    Height = 39
    Caption = 
      'Click twice and the app will tell you if both the points are on ' +
      'the same side of the line. '
    WordWrap = True
  end
  object SameSideResultLabel: TLabel
    Left = 8
    Top = 268
    Width = 33
    Height = 13
    Caption = 'Result:'
  end
  object IntersectInfoLabel: TLabel
    Left = 208
    Top = 8
    Width = 175
    Height = 26
    Caption = 
      'Click twice and the app will tell you if the two lines intersect' +
      '.'
    WordWrap = True
  end
  object IntersectImage: TImage
    Left = 208
    Top = 56
    Width = 185
    Height = 201
    OnMouseDown = IntersectImageMouseDown
  end
  object IntersectResultLabel: TLabel
    Left = 208
    Top = 268
    Width = 33
    Height = 13
    Caption = 'Result:'
  end
  object InsideInfoLabel: TLabel
    Left = 408
    Top = 8
    Width = 174
    Height = 39
    Caption = 
      'Move the cursor over the image and the app will tell you if you ' +
      'are inside the polygon'
    WordWrap = True
  end
  object InsideImage: TImage
    Left = 408
    Top = 56
    Width = 185
    Height = 201
    OnMouseMove = InsideImageMouseMove
  end
  object InsideResultLabel: TLabel
    Left = 408
    Top = 268
    Width = 33
    Height = 13
    Caption = 'Result:'
  end
  object SameSideClearButton: TButton
    Left = 120
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 0
    OnClick = SameSideClearButtonClick
  end
  object IntersectClearButton: TButton
    Left = 320
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 1
    OnClick = IntersectClearButtonClick
  end
  object InsideClearButton: TButton
    Left = 520
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Mark/unmark'
    TabOrder = 2
    OnClick = InsideClearButtonClick
  end
end
