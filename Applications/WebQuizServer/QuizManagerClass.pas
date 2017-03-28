unit QuizManagerClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Contnrs;

type
  {** The base class for a quiz manager, i.e. an object that holds available
   quizzes, with questions and answers. Derive from this class, to create a
   class capable of reading quiz information from a particular source type. A
   derived class may use the ancestor's functionality in one of two ways. Either
   it loads all available quizzes into memory when created, by calling the
   AddQuiz and AddQuestion methods, or it retrieves specific information from
   its source as it is needed. The first approach is quicker, once the
   information has been loaded into memory, but consumes a lot of internal
   memory for large quiz databases. The second approach is typically slower, but
   does not load information into memory until is is needed. }
  TQuizManager = class
  private
    Quizes: TStringList; {Objects are of type TQuiz}
    CurrentQuiz: TObject; {Is actually a TQuiz}
    procedure Clear;
    function GetQuiz(const aName: String): TObject; {Actually returns a TQuiz}
  protected

    {** If you derive from this class to create a new class that should keep all
     quiz information in memory, call this method to add quizzes. }
    procedure AddQuiz(const QuizName: String);

    {** If you derive from this class to create a new class that should keep all
     quiz information in memory, call this method to add a question to the last
     quiz added (using AddQuiz). }
    procedure AddQuestion(const Question, Alt1, Alt2, Alt3, Alt4: String;
      Correct: Integer);

    {** If you derive from this class to create a new class that loads
     information when it is needed, override this method to return the names of
     all quizzes. }
    procedure InternalGetQuizNames(const QuizNames: TStrings); virtual;

    {** If you derive from this class to create a new class that loads
     information when it is needed, override this method to return the number
     of questions for the quiz with given name. Note that if an invalid name is
     given, the function must return 0. }
    function InternalGetNumberOfQuestions(const QuizName: String): Integer; virtual;

    {** If you derive from this class to create a new class that loads
     information when it is needed, override this method to return the question
     with given index for the quiz with given name. The function should return
     True if the question information could be retrieved, False otherwise.}
    function InternalGetQuestion(const QuizName: String; QuestionIndex: Integer;
      out Question, Alt1, Alt2, Alt3, Alt4: String; out Correct: Integer):
      Boolean; virtual;
  public

    {** Fills the Names string list with the names of all available quizzes. }
    procedure GetQuizNames(const QuizNames: TStrings);

    {** Returns the number of questions for the quiz with given name. }
    function GetNumberOfQuestions(const QuizName: String): Integer;

    {** Retrieves the question, alternatives and correct alternative for the
     question with given index in the quiz with given name. Returns True if the
     question information could be retrieved, False otherwise (typically because
     of an incorrect QuizName or question index). Note that QuestionIndex should
     be between 0 and GetNumberOfQuestions(QuizName)-1. }
    function GetQuestion(const QuizName: String; QuestionIndex: Integer; out Question,
      Alt1, Alt2, Alt3, Alt4: String; out Correct: Integer): Boolean;

    constructor Create; virtual;
    destructor Destroy; override;
  end{class};

implementation

{ Note: TQuestion and TQuiz are both used by the quiz manager. I do not want to
  declare them in the "interface" section, since I do not want them to be
  accessible from other units. }
type
  TQuestion = record
    Question, Alt1, Alt2, Alt3, Alt4: String;
    Correct: Integer;
  end;
  PQuestion = ^TQuestion;

  TQuiz = class
  protected
    Questions: TList; {Of TQuestion}
    function NumberOfQuestions: Integer;
    procedure AddQuestion(const Question, Alt1, Alt2, Alt3, Alt4: String; Correct: Integer);
    function GetQuestion(Index: Integer; out Question, Alt1, Alt2, Alt3, Alt4: String;
      out Correct: Integer): Boolean;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  end{class};

{---------- TQuizManager ------------------------------------------------------}

function TQuiz.NumberOfQuestions: Integer;
begin
  Result := Questions.Count;
end;

procedure TQuiz.AddQuestion(const Question, Alt1, Alt2, Alt3, Alt4: String;
  Correct: Integer);
var
  Q: PQuestion;
begin
  New(Q);
  Q^.Question := Question;
  Q^.Alt1 := Alt1;
  Q^.Alt2 := Alt2;
  Q^.Alt3 := Alt3;
  Q^.Alt4 := Alt4;
  Q^.Correct := Correct;
  Questions.Add(Q);
end;

function TQuiz.GetQuestion(Index: Integer; out Question, Alt1, Alt2, Alt3,
  Alt4: String; out Correct: Integer): Boolean;
var
  Q: PQuestion;
begin
  try
    Q := Questions[Index];
    Question := Q^.Question;
    Alt1 := Q^.Alt1;
    Alt2 := Q^.Alt2;
    Alt3 := Q^.Alt3;
    Alt4 := Q^.Alt4;
    Correct := Q^.Correct;
    Result := True;
  except
    on E: EListError do
      Result := False
    else
      Raise;
  end;
end;

procedure TQuiz.Clear;
var
  i: Integer;
begin
  for i := 0 to Questions.Count-1 do
    Dispose(PQuestion(Questions[i]));
  Questions.Clear;
end;

constructor TQuiz.Create;
begin
  Questions := TObjectList.Create(False);
end;

destructor TQuiz.Destroy;
begin
  Clear;
  Questions.Free;
  inherited;
end;

{---------- TQuizManager ------------------------------------------------------}

procedure TQuizManager.Clear;
var
  i: Integer;
begin
  for i := 0 to Quizes.Count-1 do
    Quizes.Objects[i].Free;
end;

function TQuizManager.GetQuiz(const aName: String): TObject;
var
  idx: Integer;
begin
  idx := Quizes.IndexOf(aName);
  if idx > -1 then
    Result := Quizes.Objects[idx]
  else
    Result := nil;
end;

procedure TQuizManager.AddQuiz(const QuizName: String);
begin
  CurrentQuiz := TQuiz.Create;
  Quizes.AddObject(QuizName, CurrentQuiz);
end;

procedure TQuizManager.AddQuestion(const Question, Alt1, Alt2, Alt3,
  Alt4: String; Correct: Integer);
begin
  if Assigned(CurrentQuiz) then
    (CurrentQuiz as TQuiz).AddQuestion(Question, Alt1, Alt2, Alt3, Alt4, Correct);
end;

procedure TQuizManager.InternalGetQuizNames(const QuizNames: TStrings);
var
  i: Integer;
begin
  { Note: I do not use Assign here, since I don't want the TQuiz objects
    available in Names }
  QuizNames.Clear;
  QuizNames.BeginUpdate;
  try
    for i := 0 to Quizes.Count-1 do
      QuizNames.Add(Quizes[i]);
  finally
    QuizNames.EndUpdate;
  end;
end;

function TQuizManager.InternalGetNumberOfQuestions(const QuizName: String): Integer;
var
  Q: TQuiz;
begin
  Q := GetQuiz(QuizName) as TQuiz;
  if Assigned(Q) then
    Result := Q.NumberOfQuestions
  else
    Result := 0;
end;

function TQuizManager.InternalGetQuestion(const QuizName: String;
  QuestionIndex: Integer; out Question, Alt1, Alt2, Alt3, Alt4: String;
  out Correct: Integer): Boolean;
var
  Q: TQuiz;
begin
  Q := GetQuiz(QuizName) as TQuiz;
  if Assigned(Q) then
    Result := Q.GetQuestion(QuestionIndex, Question, Alt1, Alt2, Alt3, Alt4, Correct)
  else
    Result := False;
end;

procedure TQuizManager.GetQuizNames(const QuizNames: TStrings);
begin
  InternalGetQuizNames(QuizNames);
end;

function TQuizManager.GetNumberOfQuestions(const QuizName: String): Integer;
begin
  Result := InternalGetNumberOfQuestions(QuizName);
end;

function TQuizManager.GetQuestion(const QuizName: String; QuestionIndex: Integer;
  out Question, Alt1, Alt2, Alt3, Alt4: String; out Correct: Integer): Boolean;
begin
  Result := InternalGetQuestion(QuizName, QuestionIndex, Question, Alt1, Alt2,
    Alt3, Alt4, Correct);
end;

constructor TQuizManager.Create;
begin
  Quizes := TStringList.Create; {Containing TQuiz objects}
end;

destructor TQuizManager.Destroy;
begin
  Clear;
  Quizes.Free;
  inherited;
end;

end.

