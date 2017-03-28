unit HardcodedQuizManagerClass;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, QuizManagerClass;

type
  THardCodedQuizManager = class(TQuizManager)
  public
    constructor Create; override;
  end{class};

implementation

constructor THardCodedQuizManager.Create;
begin
  inherited;
  AddQuiz('Geography');
  AddQuestion('What is the capital of Spain?', 'Stockholm', 'Madrid', 'Rome', 'Paris', 2);
  AddQuestion('What is the capital of Sweden?', 'Stockholm', 'Madrid', 'Rome', 'Paris', 1);
  AddQuestion('What is the capital of France?', 'Stockholm', 'Madrid', 'Rome', 'Paris', 4);
  AddQuestion('What is the capital of Italy?', 'Stockholm', 'Madrid', 'Rome', 'Paris', 3);

  AddQuiz('Italian');
  AddQuestion('What does "gatto" mean?', 'Dog', 'Cat', 'Car', 'House', 2);
  AddQuestion('What does "machina" mean?', 'Dog', 'Cat', 'Car', 'House', 3);

  AddQuiz('Math');
  AddQuestion('What is 2+2?', '4', '5', '6', '7', 1);
  AddQuestion('What is 3+3?', '4', '5', '6', '7', 3);
  AddQuestion('What is 7-3?', '4', '5', '6', '7', 1);
  AddQuestion('What is 7-2?', '4', '5', '6', '7', 2);
end;

end.

