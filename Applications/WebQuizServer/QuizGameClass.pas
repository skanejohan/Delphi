unit QuizGameClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, QuizManagerClass;

type

  {** An object of this type handles a quiz game in progress. It accesses actual
   game data  - questions and answers - from an object of a class derived from
   TGameManager. }
  TQuizGame = class
  private
    FNumberOfCorrectAnswers: Integer;
    FNumberOfIncorrectAnswers: Integer;
    FNumberOfQuestions: Integer;
    function GetDone: Boolean;
  protected
    QuizName: String;
    QuizManager: TQuizManager;
    ExpectedAnswer: Integer;
    ConsumedQuestions: Array of Boolean;
  public

    {** Once you have answered the number of questions specified in Initialize,
     this property will become True. }
    property Done: Boolean read GetDone;

    {** This is the total number of questions you are expeceted to answer, as
      specified in Initialize. }
    property NumberOfQuestions: Integer read FNumberOfQuestions;

    {** The number of correct answers so far. }
    property NumberOfCorrectAnswers: Integer read FNumberOfCorrectAnswers;

    {** The number of incorrect answers so far. }
    property NumberOfIncorrectAnswers: Integer read FNumberOfIncorrectAnswers;

    {** Retrieves a random question, together with the four alternatives and the
     index (1-4) of the correct alternative. If a question could be retrieved,
     the function returns True and the correct data is placed in the out
     parameters. If not (because enough questions have already been asked; see
     Done), returns False. }
    function GetNextQuestion(out Question, Alt1, Alt2, Alt3, Alt4: String;
      out Number: Integer): Boolean;

    {** Answers (by supplying the index, 1-4, of the selected alternative) the
     previously retrieved question. If the answer was correct, returns True
     otherwise False. The index of the correct answer is in either case placed
     in CorrectAnswer. The value of either property NumberOfCorrectAnswers or
     NumberOfIncorrectAnswers is updated. }
    function Respond(Answer: Integer; out CorrectAnswer: Integer): Boolean;

    {** Call this function to initialize the object before using it. You supply
     an object derived from TQuizManager, from which this object will pick its
     questions, a name of a quiz available in the supplied quiz manager and the
     number of questions that should be asked. If initialization was successful,
     the function returns True. If not (because the selected quiz does not exist
     or because it doesn't contain enough questions), it returns False. }
    function Initialize(aQuiz: TQuizManager; const aQuizName: String;
      aNumberOfQuestions: Integer): Boolean;
  end{class};

implementation

{---------- TQuizGame ---------------------------------------------------}

function TQuizGame.GetDone: Boolean;
begin
  Result := NumberOfCorrectAnswers + NumberOfIncorrectAnswers >= NumberOfQuestions;
end{function};

function TQuizGame.GetNextQuestion(out Question, Alt1, Alt2, Alt3,
  Alt4: String; out Number: Integer): Boolean;
var
  QuestionID: Integer;
begin
  if not Done then
  begin
    { Find a random question that has not already been used }
    repeat
      QuestionID := Random(QuizManager.GetNumberOfQuestions(QuizName));
    until not ConsumedQuestions[QuestionID];

    Result := QuizManager.GetQuestion(QuizName, QuestionID, Question, Alt1, Alt2, Alt3,
      Alt4, ExpectedAnswer);
    if Result then
    begin
      ConsumedQuestions[QuestionID] := True;
      Number := FNumberOfCorrectAnswers + FNumberOfIncorrectAnswers + 1;
    end{if};
  end{if}
  else
    Result := False;
end{function};

function TQuizGame.Respond(Answer: Integer; out CorrectAnswer: Integer): Boolean;
begin
  Result := Answer = ExpectedAnswer;
  CorrectAnswer := ExpectedAnswer;
  if Result then
    Inc(FNumberOfCorrectAnswers)
  else
    Inc(FNumberOfIncorrectAnswers);
end{function};

function TQuizGame.Initialize(aQuiz: TQuizManager; const aQuizName: String;
  aNumberOfQuestions: Integer): Boolean;
var
  i: Integer;
begin
  Result := aQuiz.GetNumberOfQuestions(aQuizName) >= aNumberOfQuestions;
  if Result then
  begin
    QuizManager := aQuiz;
    QuizName := aQuizName;
    FNumberOfQuestions := aNumberOfQuestions;
    FNumberOfCorrectAnswers := 0;
    FNumberOfIncorrectAnswers := 0;
    SetLength(ConsumedQuestions, FNumberOfQuestions);
    for i := 0 to FNumberOfQuestions-1 do
      ConsumedQuestions[i] := False;
  end{if};
end{function};

end.

