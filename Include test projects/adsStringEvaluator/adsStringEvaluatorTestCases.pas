unit adsStringEvaluatorTestCases;

interface

uses
  TestFrameWork, Classes, adsStringEvaluatorClass;

type
  TadsStringEvaluatorTestCase = class(TTestCase)
  protected
    Strings: TStrings;
    Evaluator: TadsStringEvaluator;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CreateAndDescribe(const AText: String);
    function Evaluate(const Expr: String): Boolean;
  published
    procedure TestIncorrectInputString;
    procedure TestOrPerfectMatch;
    procedure TestAndPerfectMatch;
    procedure TestNotPerfectMatch;
    procedure TestOrPartialMatch;
    procedure TestAndPartialMatch;
    procedure TestNotPartialMatch;
    procedure TestDocumentedExamples;
    procedure TestComplex;
    procedure TestExternalEvaluation;
  end{class};

implementation

uses
  Forms, SysUtils, adsLoggerClass, adsObjectClass;

procedure TadsStringEvaluatorTestCase.SetUp;
begin
  inherited;
  Strings := TStringList.Create;
  Evaluator := TadsStringEvaluator.Create;
end{procedure};

procedure TadsStringEvaluatorTestCase.TearDown;
begin
  Evaluator.Free;
  Strings.Free;
  inherited;
end{procedure};

procedure TadsStringEvaluatorTestCase.CreateAndDescribe(const AText: String);
var
  i: Integer;
  Tree: TStrings;
begin
  Tree := TStringList.Create;
  try
    Evaluator.CreateBooleanTree(AText);
    LogDebug(Self, 'Expression: ' + AText);
    Evaluator.DescribeTree(Tree);
    for i := 0 to Tree.Count-1 do
      LogDebug(Self, '..Tree...' + Tree[i]);
  finally
    Tree.Free;
  end{finally};
end{procedure};

function TadsStringEvaluatorTestCase.Evaluate(const Expr: String): Boolean;
begin
  Result :=  Length(Strings.CommaText) = StrToInt(Expr);
end{function};

procedure TadsStringEvaluatorTestCase.TestIncorrectInputString;
begin
  try
    Evaluator.CreateBooleanTree('Sweden Denmark');
    Check(False);
  except
    on E: Exception do
    begin
      CheckEquals('EParseError', E.ClassName);
      CheckEquals('Unexpected token "Denmark" in line 1', E.Message);
    end{on};
  end{except};
end{procedure};

procedure TadsStringEvaluatorTestCase.TestOrPerfectMatch;
begin
  Evaluator.CreateBooleanTree('Sweden or Denmark');
  Check(not Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate(''));
  Strings.Add('Germany');
  Check(not Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate('Germany'));
  Strings.Add('Denmark');
  Check(Evaluator.Evaluate(Strings));
  Check(Evaluator.Evaluate('Denmark'));
  Strings.Clear;
  Strings.Add('denmark');
  Check(Evaluator.Evaluate(Strings));
  Check(Evaluator.Evaluate('denmark'));
  Strings.Clear;
  Strings.Add('Sweden');
  Check(Evaluator.Evaluate(Strings));
  Check(Evaluator.Evaluate('Sweden'));
  Strings.Clear;
  Strings.Add('sweden');
  Check(Evaluator.Evaluate(Strings));
  Check(Evaluator.Evaluate('sweden'));
  Strings.Clear;
  Strings.Add('swedens');
  Check(not Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate('swedens'));
end{procedure};

procedure TadsStringEvaluatorTestCase.TestAndPerfectMatch;
begin
  Evaluator.CreateBooleanTree('Sweden and Denmark');
  Check(not Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate(''));
  Strings.Add('Germany');
  Check(not Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate(''));
  Strings.Add('Denmark');
  Check(not Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate('Denmark'));
  Strings.Add('Sweden');
  Check(Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate('Sweden'));
  Strings.Clear;
  Strings.Add('denmark');
  Check(not Evaluator.Evaluate(Strings));
  Strings.Add('swedens');
  Check(not Evaluator.Evaluate(Strings));
  Strings.Add('sweden');
  Check(Evaluator.Evaluate(Strings));
end{procedure};

procedure TadsStringEvaluatorTestCase.TestNotPerfectMatch;
begin
  Evaluator.CreateBooleanTree('not Sweden');
  Check(Evaluator.Evaluate(Strings));
  Check(Evaluator.Evaluate(''));
  Strings.Add('Germany');
  Check(Evaluator.Evaluate(Strings));
  Check(Evaluator.Evaluate('Germany'));
  Strings.Add('Denmark');
  Check(Evaluator.Evaluate(Strings));
  Check(Evaluator.Evaluate('Denmark'));
  Strings.Add('Sweden');
  Check(not Evaluator.Evaluate(Strings));
  Check(not Evaluator.Evaluate('Sweden'));
  Strings.Clear;
  Strings.Add('Sweden');
  Check(not Evaluator.Evaluate(Strings));
  Strings.Clear;
  Strings.Add('Swedens');
  Check(Evaluator.Evaluate(Strings));
end{procedure};

procedure TadsStringEvaluatorTestCase.TestOrPartialMatch;
begin
  Evaluator.CreateBooleanTree('Sweden or Denmark');
  Check(not Evaluator.Evaluate(Strings, False));
  Check(not Evaluator.Evaluate('', False));
  Strings.Add('Germany');
  Check(not Evaluator.Evaluate(Strings, False));
  Check(not Evaluator.Evaluate('Germany', False));
  Strings.Add('Denmark');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('Denmark', False));
  Strings.Clear;
  Strings.Add('denmark');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('denmark', False));
  Strings.Clear;
  Strings.Add('Sweden');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('Sweden', False));
  Strings.Clear;
  Strings.Add('sweden');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('sweden', False));
  Strings.Clear;
  Strings.Add('swedens');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('swedens', False));
end{procedure};

procedure TadsStringEvaluatorTestCase.TestAndPartialMatch;
begin
  Evaluator.CreateBooleanTree('Sweden and Denmark');
  Check(not Evaluator.Evaluate(Strings, False));
  Check(not Evaluator.Evaluate('', False));
  Strings.Add('Germany');
  Check(not Evaluator.Evaluate(Strings, False));
  Check(not Evaluator.Evaluate('', False));
  Strings.Add('Denmark');
  Check(not Evaluator.Evaluate(Strings, False));
  Check(not Evaluator.Evaluate('Denmark', False));
  Strings.Add('Sweden');
  Check(Evaluator.Evaluate(Strings, False));
  Check(not Evaluator.Evaluate('Sweden', False));
  Strings.Clear;
  Strings.Add('denmark');
  Check(not Evaluator.Evaluate(Strings, False));
  Strings.Add('swedens');
  Check(Evaluator.Evaluate(Strings, False));
  Strings.Add('sweden');
  Check(Evaluator.Evaluate(Strings, False));
end{procedure};

procedure TadsStringEvaluatorTestCase.TestNotPartialMatch;
begin
  Evaluator.CreateBooleanTree('not Sweden');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('', False));
  Strings.Add('Germany');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('Germany', False));
  Strings.Add('Denmark');
  Check(Evaluator.Evaluate(Strings, False));
  Check(Evaluator.Evaluate('Denmark', False));
  Strings.Add('Sweden');
  Check(not Evaluator.Evaluate(Strings, False));
  Check(not Evaluator.Evaluate('Sweden', False));
  Strings.Clear;
  Strings.Add('Sweden');
  Check(not Evaluator.Evaluate(Strings, False));
  Strings.Clear;
  Strings.Add('Swedens');
  Check(not Evaluator.Evaluate(Strings, False));

end{procedure};

procedure TadsStringEvaluatorTestCase.TestDocumentedExamples;
begin
  Evaluator.CreateBooleanTree('Java and (not Beans)');
  Strings.CommaText := 'Java';
  Check(Evaluator.Evaluate(Strings, True));
  Strings.CommaText := 'JavaScript';
  Check(not Evaluator.Evaluate(Strings, True));
  Strings.CommaText := 'Java';
  Check(Evaluator.Evaluate(Strings, False));
  Strings.CommaText := 'JavaScript';
  Check(Evaluator.Evaluate(Strings, False));
  Strings.CommaText := 'JavaBeans';
  Check(not Evaluator.Evaluate(Strings, False));
  Strings.CommaText := 'Java,Script';
  Check(Evaluator.Evaluate(Strings, True));
  Strings.CommaText := 'Java,JavaBeans';
  Check(Evaluator.Evaluate(Strings, True));
  Check(not Evaluator.Evaluate(Strings, False));
end{procedure};

procedure TadsStringEvaluatorTestCase.TestComplex;
begin
  Evaluator.CreateBooleanTree('A and (not B) and not ((C and D) or E)');
  Strings.CommaText := 'A,C';
  Check(Evaluator.Evaluate(Strings));
  Strings.CommaText := 'A,C,D';
  Check(not Evaluator.Evaluate(Strings));
  Strings.CommaText := 'A,E';
  Check(not Evaluator.Evaluate(Strings));
  Strings.CommaText := 'A,B';
  Check(not Evaluator.Evaluate(Strings));
  Strings.CommaText := 'A';
  Check(Evaluator.Evaluate(Strings));
end{procedure};

procedure TadsStringEvaluatorTestCase.TestExternalEvaluation;
begin
  Evaluator.ExternalEvaluator := Evaluate;
  CreateAndDescribe('A or @"7" or @11');
  Strings.CommaText := 'aa,bbb,cccc';
  Check(Evaluator.Evaluate(Strings)); // Length(Strings.CommaText)=11 => OK
  Strings.CommaText := 'aa,bbb,';
  Check(Evaluator.Evaluate(Strings)); // Length(Strings.CommaText)=7 => OK
  Strings.CommaText := 'aa,bbb,c';
  Check(not Evaluator.Evaluate(Strings)); // Length(Strings.CommaText)=8 => not OK
  Strings.CommaText := 'A,bbbb,cc';
  Check(Evaluator.Evaluate(Strings)); // Comtains "A" => OK
end;

initialization
  TestFramework.RegisterTest(TadsStringEvaluatorTestCase.Suite);
  Logger.AddSubscription(ChangeFileExt(Application.ExeName, '.log'), [ltError,
    ltWarning, ltInformation, ltDebug, ltInsane, ltMemDebug, ltMemError]);
  LogCreateAndDestroy := True;

end.
