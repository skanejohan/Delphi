unit adsStringEvaluatorClass;

{ © 2010 Aston Design Studio }

{
  The TadsStringEvaluator class can be used to build a Boolean parse tree based
  on an input string, and then match one or more strings against it to see if
  they pass. The grammar used in the string defining the rules supports the
  binary operators "and" and "or", as well as the unary operator "not". It also
  supports parentheses to determine the order in which the operators are to be
  applied. In addition to this, the grammar supports a special operator, "@".
  When the "@" operator is encountered, the text immediately following it
  (e.g. 'Java' in the expression @Java or "Tom and Jerry" in the expression
  @"Tom and Jerry") is interpreted as a string, but is it not matched against
  the strings. Instead, to determine whether the string in question is valid,
  the ExternalEvaluator method is called, which should determine this.

  BNF grammar:

    Expr  ::= '(' Expr ')'
          |   Expr BinOp Expr
          |   UnOp Expr
          |   String

    BinOp ::= AND | OR

    UnOp  ::= NOT
}

interface

uses
  Classes, SysUtils, Contnrs, adsObjectClass, adsParserClass;

type
  EParseError = class(Exception);

  TExternalStringEvaluatorFunc = function(const Expr: String): Boolean of object;

  {** TExpression and its derived classes replace the tokens when the parse
      tree is created. }
  TExpression = class(TadsObject)
  protected
    Text: String;
  public
    procedure Describe(const Strings: TStrings; Indent: Integer=0); virtual;
    function Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean; virtual; abstract;
  end; {class}

  TStringExpr = class(TExpression)
  public
    procedure Describe(const Strings: TStrings; Indent: Integer=0); override;
    function Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean; override;
    constructor Create(const S: String);
  end; {class}

  TExternalExpr = class(TExpression)
  protected
    Evaluator: TExternalStringEvaluatorFunc;
  public
    procedure Describe(const Strings: TStrings; Indent: Integer=0); override;
    function Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean; override;
    constructor Create(const S: String; ExternalEvaluator: TExternalStringEvaluatorFunc);
  end; {class}

  TBinOpExpr = class(TExpression)
  protected
    LeftExpr, RightExpr: TExpression;
  public
    procedure Describe(const Strings: TStrings; Indent: Integer=0); override;
    function Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean; override;
    constructor Create(Left: TExpression; const BinOp: String; Right: TExpression);
    destructor Destroy; override;
  end; {class}

  TUnOpExpr = class(TExpression)
  protected
    RightExpr: TExpression;
  public
    procedure Describe(const Strings: TStrings; Indent: Integer=0); override;
    function Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean; override;
    constructor Create(const UnOp: String; Right: TExpression);
    destructor Destroy; override;
  end; {class}

  {** The class that handles creation of the boolean search tree and evaluation. }
  TadsStringEvaluator = class(TadsParser)
  private
    FExternalEvaluator: TExternalStringEvaluatorFunc;
  protected
    ParseTree: TExpression;
    function NextIsUnOp: Boolean;
    function NextIsBinOp: Boolean;
    function NextIsLeftPar: Boolean;
    function NextIsRightPar: Boolean;
    function ParseBinaryOperator: TExpression;
    function ParseUnaryOperator: TExpression;
    function ParseParentheses: TExpression;
    function ParsePrimitive: TExpression;
  public

    {** Assign this method if you want external evaluation of certain strings. }
    property ExternalEvaluator: TExternalStringEvaluatorFunc
      read FExternalEvaluator write FExternalEvaluator;

    {** Call this method to create a boolean tree. The AText parameter is a
        string containg a boolean expression on strings, i.e. it can contain
        the reserved words "and", "or" and "not" and left and right parentheses
        in addition to any valid string. }
    procedure CreateBooleanTree(AText: String);

    {** Get a description of the tree. For testing purposes. }
    procedure DescribeTree(const Strings: TStrings);

    {** Call evaluate to determine whether a string or a number of strings are
     valid in the created boolean tree.

     Examples: assuming a Boolean expression like "Java and (not Beans)", the
      following will happen:
      Evaluate(['Java'], True) -> True
      Evaluate(['JavaScript'], True) -> False
      Evaluate(['Java'], False) -> True
      Evaluate(['JavaScript'], False) -> True
      Evaluate(['JavaBeans'], False) -> False
      Evaluate(['Java';'Script'], True) -> True
      Evaluate(['Java';'JavaBeans'], True) -> True
      Evaluate(['Java';'JavaBeans'], False) -> False

     Note: evaluating one string follows the same rules as evauating a list
     containing only one string, i.e. Evaluate(['Java'], True) is the same as
     Evaluate('Java', True).

     If the PerfectMatch variable is True, the word "Swedens" will not match
     against "Sweden", but if it is False, it will. 
    }
    function Evaluate(Strings: TStrings; PerfectMatch: Boolean = True): Boolean; overload;
    function Evaluate(S: String; PerfectMatch: Boolean = True): Boolean; overload;

    constructor Create; override;
    destructor Destroy; override;
  end; {class}

implementation

{---------- Implementation of expression classes ----------}

procedure TExpression.Describe(const Strings: TStrings; Indent: Integer=0);
begin
  Strings.Add('ERROR: TExpression should not be instantiated!');
end; {procedure}

constructor TStringExpr.Create(const S: String);
begin
  inherited Create;
  Text := S;
end; {constructor}

procedure TStringExpr.Describe(const Strings: TStrings; Indent: Integer=0);
begin
  Strings.Add(StringOfChar(' ', Indent)+Text);
end; {procedure}

function TStringExpr.Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean;
begin
  if PerfectMatch then
    Result := Strings.IndexOf(Text) <> -1
  else
    Result := Pos(LowerCase(Text), LowerCase(Strings.Text)) <> 0;
end; {function}

procedure TExternalExpr.Describe(const Strings: TStrings; Indent: Integer=0);
begin
  Strings.Add(StringOfChar(' ', Indent)+Text+' (External)');
end; {procedure}

function TExternalExpr.Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean;
begin
  Result := Evaluator(Text);
end; {function}

constructor TExternalExpr.Create(const S: String; ExternalEvaluator:
  TExternalStringEvaluatorFunc);
begin
  inherited Create;
  if S[1] = '"' then
    Text := Copy(S, 2, Length(S)-2)
  else
    Text := S;
  Evaluator := ExternalEvaluator;
end; {constructor}

constructor TBinOpExpr.Create(Left: TExpression; const BinOp: String; Right: TExpression);
begin
  inherited Create;
  LeftExpr := Left;
  Text := BinOp;
  RightExpr := Right;
end; {constructor}

destructor TBinOpExpr.Destroy;
begin
  LeftExpr.Free;
  RightExpr.Free;
  inherited;
end; {destructor}

procedure TBinOpExpr.Describe(const Strings: TStrings; Indent: Integer=0);
begin
  Strings.Add(StringOfChar(' ', Indent)+Text);
  LeftExpr.Describe(Strings, Indent+2);
  RightExpr.Describe(Strings, Indent+2);
end; {procedure}

function TBinOpExpr.Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean;
begin
  if AnsiSameText(Text, 'and') then
    Result := LeftExpr.Evaluate(Strings, PerfectMatch) and RightExpr.Evaluate(Strings, PerfectMatch)
  else
    Result := LeftExpr.Evaluate(Strings, PerfectMatch) or RightExpr.Evaluate(Strings, PerfectMatch);
end; {function}

constructor TUnOpExpr.Create(const UnOp: String; Right: TExpression);
begin
  inherited Create;
  Text := UnOp;
  RightExpr := Right;
end; {constructor}

destructor TUnOpExpr.Destroy;
begin
  RightExpr.Free;
  inherited;
end; {destructor}

procedure TUnOpExpr.Describe(const Strings: TStrings; Indent: Integer=0);
begin
  Strings.Add(StringOfChar(' ', Indent)+Text);
  RightExpr.Describe(Strings, Indent+2);
end; {procedure}

function TUnOpExpr.Evaluate(const Strings: TStrings; PerfectMatch: Boolean): Boolean;
begin
  if AnsiSameText(Text, 'not') then
    Result := not RightExpr.Evaluate(Strings, PerfectMatch)
  else
    Raise EParseError.Create('"Not" is the only allowed unary operator');
end; {function}

{---------- Implementation of TadsStringEvaluator -----------------------------}

function TadsStringEvaluator.NextIsUnOp: Boolean;
begin
  Result := NextTokenEquals('NOT');
end{function};

function TadsStringEvaluator.NextIsBinOp: Boolean;
begin
  Result := NextTokenEquals('AND') or NextTokenEquals('OR');
end{function};

function TadsStringEvaluator.NextIsLeftPar: Boolean;
begin
  Result := NextTokenEquals('(');
end{function};

function TadsStringEvaluator.NextIsRightPar: Boolean;
begin
  Result := NextTokenEquals(')');
end{function};

function TadsStringEvaluator.ParseBinaryOperator: TExpression;
var
  Operator: String;
  Left, Right: TExpression;
begin
  Left := ParseUnaryOperator;
  if ThereAreMoreTokens then
  begin
    while NextIsBinOp do
    begin
      Operator := ParseNext;
      Right := ParseUnaryOperator;
      Left := TBinOpExpr.Create(Left, Operator, Right);
    end; {while}
  end; {if}
  Result := Left;
end; {function}

function TadsStringEvaluator.ParseUnaryOperator: TExpression;
var
  Operator: String;
  SubExpression: TExpression;
begin
  if NextTokenEquals('NOT') then
    Operator := ParseNext
  else
    Operator := '';

  SubExpression := ParseParentheses;
  if Operator = '' then
    Result := SubExpression
  else
    Result := TUnOpExpr.Create(Operator, SubExpression);
end; {function}

function TadsStringEvaluator.ParseParentheses: TExpression;
begin
  if NextTokenEquals('(') then
  begin
    ParseNext;
    Result := ParseBinaryOperator;
    if NextTokenEquals(')') then
      ParseNext
    else
      DoErrorInvalidToken(')');
  end{if}
  else
    Result := ParsePrimitive;
end; {function}

function TadsStringEvaluator.ParsePrimitive: TExpression;
var
  S: String;
begin
  {The following line avoids a warning that occurs because the compiler can't
   see that DoErrorInvalidToken unconditionally raises an exception. }
  Result := nil;
  if not (NextIsUnOp or NextIsBinOp or NextIsLeftPar or NextIsRightPar) then
  begin
    S := ParseNext;
    if S = '@' then
    begin
      S := ParseNext;
      Result := TExternalExpr.Create(S, FExternalEvaluator);
    end{if}
    else
      Result := TStringExpr.Create(S);
  end {if}
  else
    DoErrorInvalidToken('(primitive)');
end; {function}

procedure TadsStringEvaluator.CreateBooleanTree(AText: String);
var
  Text: TStrings;
begin
  Text := TStringList.Create;
  try
    Text.Add(AText);
    Tokenize(Text, ['(', ')', '@'], [], [], []);
    ParseTree.Free;
    ParseTree := ParseBinaryOperator;
    if ThereAreMoreTokens then
      DoErrorUnexpectedToken;
  finally
    Text.Free;
  end; {try/finally}
end; {procedure}

procedure TadsStringEvaluator.DescribeTree(const Strings: TStrings);
begin
  Strings.Clear;
  if ParseTree <> nil then
    ParseTree.Describe(Strings);
end; {procedure}

function TadsStringEvaluator.Evaluate(Strings: TStrings; PerfectMatch: Boolean = True): Boolean;
begin
  if ParseTree <> nil then
    Result := ParseTree.Evaluate(Strings, PerfectMatch)
  else
    Result := False;
end; {procedure}

function TadsStringEvaluator.Evaluate(S: String; PerfectMatch: Boolean = True): Boolean;
var
  Strings: TStrings;
begin
  Strings := TStringList.Create;
  try
    Strings.Add(S);
    Result := Evaluate(Strings, PerfectMatch);
  finally
    Strings.Free;
  end{finally};
end{function};

constructor TadsStringEvaluator.Create;
begin
  inherited;
  ParseTree := nil;
  FExternalEvaluator := nil;
end; {constructor}

destructor TadsStringEvaluator.Destroy;
begin
  ParseTree.Free;
  inherited;
end; {destructor}

end.
