unit WinQuizGameFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, QuizManagerClass, HardcodedQuizManagerClass,
  IniFileQuizManagerClass, QuizGameClass;

type

  { TWinQuizGameForm }

  TWinQuizGameForm = class(TForm)
    Alt1Button: TButton;
    Alt2Button: TButton;
    Alt3Button: TButton;
    Alt4Button: TButton;
    HardcodedRadioButton: TRadioButton;
    IniFileRadioButton: TRadioButton;
    ResultPanel: TPanel;
    QuestionNumberLabel: TLabel;
    QuestionLabel: TLabel;
    QuizPanel: TPanel;
    StartQuizButton: TButton;
    QuizesListBox: TListBox;
    procedure AltButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButtonChange(Sender: TObject);
    procedure StartQuizButtonClick(Sender: TObject);
  protected
    HardCodedQuizManager: THardCodedQuizManager;
    IniFileQuizManager: TIniFileQuizManager;
    UsedQuizManager: TQuizManager;
    QuizGame: TQuizGame;
    procedure PresentResult;
    procedure PresentNextQuestion;
  end;

var
  WinQuizGameForm: TWinQuizGameForm;

implementation

{ TWinQuizGameForm }

procedure TWinQuizGameForm.PresentResult;
begin
  ResultPanel.Caption := Format('Game over. Correct: %d. Incorrect: %d',
    [QuizGame.NumberOfCorrectAnswers, QuizGame.NumberOfIncorrectAnswers]);
end;

procedure TWinQuizGameForm.PresentNextQuestion;
var
  N: Integer;
  Q, A1, A2, A3, A4: String;
begin
  if QuizGame.GetNextQuestion(Q, A1, A2, A3, A4, N) then
  begin
    QuestionNumberLabel.Caption := Format('Question number %d (correct so far: %d)',
      [N, QuizGame.NumberOfCorrectAnswers]);
    QuestionLabel.Caption := Q;
    Alt1Button.Caption := A1;
    Alt2Button.Caption := A2;
    Alt3Button.Caption := A3;
    Alt4Button.Caption := A4;
  end
  else
    ShowMessage('Error!');
end{procedure};

procedure TWinQuizGameForm.StartQuizButtonClick(Sender: TObject);
var
  QuizName: String;
begin
  QuizName := QuizesListBox.Items[QuizesListBox.ItemIndex];
  QuizGame.Initialize(UsedQuizManager, QuizName,
    UsedQuizManager.GetNumberOfQuestions(QuizName));
  ResultPanel.Visible := False;
  QuizPanel.Visible := True;
  PresentNextQuestion;
end;

procedure TWinQuizGameForm.AltButtonClick(Sender: TObject);
var
  Correct: Integer;
begin
  QuizGame.Respond((Sender as TButton).Tag, Correct);
  if QuizGame.Done then
  begin
    ResultPanel.Visible := True;
    QuizPanel.Visible := False;
    PresentResult;
  end
  else
    PresentNextQuestion;
end;

procedure TWinQuizGameForm.RadioButtonChange(Sender: TObject);
begin
  if HardcodedRadioButton.Checked then
    UsedQuizManager := HardCodedQuizManager
  else
    UsedQuizManager := IniFileQuizManager;;
  UsedQuizManager.GetQuizNames(QuizesListBox.Items);
  QuizesListBox.ItemIndex := 0;
end;

procedure TWinQuizGameForm.FormCreate(Sender: TObject);
begin
  HardCodedQuizManager := THardCodedQuizManager.Create;
  IniFileQuizManager := TIniFileQuizManager.Create;
  QuizGame := TQuizGame.Create;
  RadioButtonChange(HardCodedQuizManager);
end;

procedure TWinQuizGameForm.FormDestroy(Sender: TObject);
begin
  QuizGame.Free;
  HardCodedQuizManager.Free;
  IniFileQuizManager.Free;
end;

initialization
  {$I WinQuizGameFrm.lrs}

end.

