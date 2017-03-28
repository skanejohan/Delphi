unit adsParserClass;

{ © 2010 Aston Design Studio }

interface

uses
  Classes, SysUtils, adsObjectClass, adsLoggerClass, adsTokenizerClass;

type
  EParseError = class(Exception);
  TObjectProcedure = procedure of object;

  {** Base class for a parser. Internally, it uses a TadsTokenizer object to
   split the source up into tokens. The class has o complete parsing support
   (there is no defined "Parse" method, but miscellaneous provides help methods.
   When the source has been tokenized, the TokenIndex variable is set to point
   at the first token in the tokenizaer's token list. After this, you may use
   the built-in methods to parse the tokens. }
  TadsParser = class(TadsObject)
  private
    Tokenizer: TadsTokenizer;
    FBlockBegin: String;
    FBlockEnd: String;
  protected
    TokenIndex: Integer;

    {** Logs (information) the given message to the global logger object. }
    procedure DoLog(const Msg: String);

    {** Logs (error) the given message to the global logger object, then raises
     an EParseError exception, supplying the given message. }
    procedure DoError(const Msg: String);

    {** Calls DoError with the following message: "Exception (E.ClassName) with
     message "(E.Message)" raised when Msg. }
    procedure DoErrorException(E: Exception; const Msg: String);

    {** Calls DoError with a message indicating that an invalid token was
     encountered. The log entry includes the found token, the source line and,
     if available, the expected token. }
    procedure DoErrorInvalidToken(const Expected: String = '');

    {** Calls DoError with a message indicating that an unexpected token was
     encountered. The log entry includes the found token and the source line. }
    procedure DoErrorUnexpectedToken;

    {** Calls DoError with a message indicating that a the parser expected more
     tokens after parsing all tokens in the source. }
    procedure DoErrorPrematureEnd(const Parsing: String);

    {** Returns True if there are more tokens. Does not change the token index. }
    function ThereAreMoreTokens: Boolean;

    {** Returns True if the next equals (case insensitive) Expected, False
     otherwise. If there are no more tokens, it returns False.  }
    function NextTokenEquals(const Expected: String): Boolean;

    {** Returns the source code line number of the next token, or -1 if there
     are no more tokens. }
    function NextTokenLine: Integer;

    {** Returns the next token and increases the token index. If there are no
     more tokens, raises an EParseError exception. }
    function ParseNext: String; overload;

    {** This version of ParseNext also returns the line number of the parsed
     token, in the Line parameter. }
    function ParseNext(out Line: Integer): String; overload;

    {** Verifies that the next token equals the expected value (case insensitive)
     and increases the token index. If it is not a match, or if there are no
     more tokens to parse, an EParseError exception is raised. }
    procedure ParseExpected(const Expected: String);

    {** Calls ParseExpected, supplying BlockBegin. }
    procedure ParseBeginBlock;

    {** If the next token equals BlockEnd, returns True and increases the token
     index. If not, returns False and leaves the token index unchanged. }
    function ParseEndBlock: Boolean;

    {** Tries to parse a key/value pair, typically "key=value;". If successful,
     returns True and returns the value in the Value parameter. If not
     successful, returns False. }
    function ParseKeyValue(const Key: String; var Value: String; const Operator:
      String = '='; const TrailingDelimiter: String = ';';
      UnquoteValue: Boolean = True): Boolean; overload;
    function ParseKeyValue(const Key: String; var Value: Boolean; const Operator:
      String = '='; const TrailingDelimiter: String = ';'): Boolean; overload;
    function ParseKeyValue(const Key: String; var Value: Integer; const Operator:
      String = '='; const TrailingDelimiter: String = ';'): Boolean; overload;

    {** If the next token equals (case insensitive) BlockName, increases the
     token index, executes Parser and then (if assigned) NextParser and returns
     True. If the next token does not equal BlockName, returns False. }
    function ExecuteBlockParser(const BlockName: String; Parser: TObjectProcedure;
      NextParser: TObjectProcedure=nil): Boolean;

  public

    {** The text that begins a block of code. Default is '{'. }
    property BlockBegin: String read FBlockBegin write FBlockBegin;

    (** The text that ends a block of code. Default is '}'. *)
    property BlockEnd: String read FBlockEnd write FBlockEnd;

    {** Calls the tokenizer's Tokenize method and resets the token index. }
    procedure Tokenize(Source: TStrings; Separators, SingleLineCommentMarkers,
      MultiLineCommentStartMarkers, MultiLineCommentEndMarkers: Array of String);

    constructor Create; virtual;
    destructor Destroy; override;
  end{class};

implementation

procedure TadsParser.DoLog(const Msg: String);
begin
  LogInformation(Self, Msg);
end{procedure};

procedure TadsParser.DoError(const Msg: String);
begin
  LogError(Self, Msg);
  Raise EParseError.Create(Msg);
end{procedure};

procedure TadsParser.DoErrorException(E: Exception; const Msg: String);
begin
  DoError(Format('Exception %s with message "%s" raised when %s', [E.ClassName,
    E.Message, Msg]));
end{procedure};

procedure TadsParser.DoErrorInvalidToken(const Expected: String);
begin
  if Expected = '' then
    DoError(Format('Invalid token "%s" in line %d', [
      Tokenizer.Tokens[TokenIndex].Token,
      Tokenizer.Tokens[TokenIndex].Line]))
  else
    DoError(Format('Invalid token "%s" in line %d. Expected "%s"', [
      Tokenizer.Tokens[TokenIndex].Token,
      Tokenizer.Tokens[TokenIndex].Line,
      Expected]));
end{procedure};

procedure TadsParser.DoErrorUnexpectedToken;
begin
  DoError(Format('Unexpected token "%s" in line %d', [
    Tokenizer.Tokens[TokenIndex].Token,
    Tokenizer.Tokens[TokenIndex].Line]));
end{procedure};

procedure TadsParser.DoErrorPrematureEnd(const Parsing: String);
begin
  DoError(Format('Unexpected end of file when parsing ', [Parsing]));
end{procedure};

function TadsParser.ThereAreMoreTokens: Boolean;
begin
  Result := TokenIndex < Tokenizer.TokenCount;
end{function};

function TadsParser.NextTokenEquals(const Expected: String): Boolean;
begin
  if TokenIndex < Tokenizer.TokenCount then
    Result := SameText(Tokenizer.Tokens[TokenIndex].Token, Expected)
  else
    Result := False;
end{function};

function TadsParser.NextTokenLine: Integer;
begin
  if TokenIndex < Tokenizer.TokenCount then
    Result := Tokenizer.Tokens[TokenIndex].Line
  else
    Result := -1;
end{function};

procedure TadsParser.ParseExpected(const Expected: String);
begin
  if NextTokenEquals(Expected) then
    Inc(TokenIndex)
  else if ThereAreMoreTokens then
    DoErrorInvalidToken(Expected)
  else
    DoErrorPrematureEnd(Expected);
end{procedure};

function TadsParser.ParseNext: String;
var
  Line: Integer;
begin
  Result := ParseNext(Line);
end{function};

function TadsParser.ParseNext(out Line: Integer): String;
begin
  if TokenIndex < Tokenizer.TokenCount then
  begin
    Result := Tokenizer.Tokens[TokenIndex].Token;
    Line := Tokenizer.Tokens[TokenIndex].Line;
    Inc(TokenIndex);
  end{if}
  else
    DoErrorPrematureEnd('(unknown)');
end{function};

procedure TadsParser.ParseBeginBlock;
begin
  ParseExpected(FBlockBegin);
end{function};

function TadsParser.ParseEndBlock: Boolean;
begin
  if NextTokenEquals(FBlockEnd) then
  begin
    Inc(TokenIndex);
    Result := True;
  end{if}
  else
    Result := False;
end{function};

function TadsParser.ParseKeyValue(const Key: String; var Value: String;
  const Operator: String = '='; const TrailingDelimiter: String = ';';
  UnquoteValue: Boolean = True): Boolean;
var
  Line: Integer;
begin
  if NextTokenEquals(Key) then
  begin
    Inc(TokenIndex);
    ParseExpected(Operator);
    Value := ParseNext(Line);
    if UnquoteValue and (Value <> '') and (Value[1] = '"') and
      (Value[Length(Value)] = '"') then
        Value := Copy(Value, 2, Length(Value)-2);
    DoLog(Format('%s=%s', [Key, Value]));
    if TrailingDelimiter <> '' then
      ParseExpected(TrailingDelimiter);
    Result := True;
  end{if}
  else
    Result := False;
end{function};

function TadsParser.ParseKeyValue(const Key: String; var Value: Boolean;
  const Operator: String = '='; const TrailingDelimiter: String = ';'): Boolean;
var
  S: String;
  Line: Integer;
begin
  if NextTokenEquals(Key) then
  begin
    Inc(TokenIndex);
    ParseExpected(Operator);
    S := ParseNext(Line);
    try
      Value := StrToBool(S);
      DoLog(Format('%s=%s', [Key, S]));
    except
      DoError(Format('Can not convert "%s" to Boolean in line %d', [S, Line]));
    end{except};
    if TrailingDelimiter <> '' then
      ParseExpected(TrailingDelimiter);
    Result := True;
  end{if}
  else
    Result := False;
end{function};

function TadsParser.ParseKeyValue(const Key: String; var Value: Integer;
  const Operator: String = '='; const TrailingDelimiter: String = ';'): Boolean;
var
  S: String;
  Line: Integer;
begin
  if NextTokenEquals(Key) then
  begin
    Inc(TokenIndex);
    ParseExpected(Operator);
    S := ParseNext(Line);
    try
      Value := StrToInt(S);
      DoLog(Format('%s=%s', [Key, S]));
    except
      DoError(Format('Can not convert "%s" to Integer in line %d', [S, Line]));
    end{except};
    if TrailingDelimiter <> '' then
      ParseExpected(TrailingDelimiter);
    Result := True;
  end{if}
  else
    Result := False;
end{function};

function TadsParser.ExecuteBlockParser(const BlockName: String;
  Parser: TObjectProcedure; NextParser: TObjectProcedure=nil): Boolean;
begin
  if NextTokenEquals(BlockName) then
  begin
    Inc(TokenIndex);
    DoLog('Parsing ' + BlockName);
    if TokenIndex < Tokenizer.TokenCount then
    begin
      Parser;
      DoLog(BlockName + ' parsed');
      if Assigned(NextParser) then
        NextParser;
    end{if}
    else
      DoErrorPrematureEnd(BlockName);
    Result := True;
  end{if}
  else
    Result := False;
end{function};

procedure TadsParser.Tokenize(Source: TStrings; Separators,
  SingleLineCommentMarkers, MultiLineCommentStartMarkers,
  MultiLineCommentEndMarkers: array of String);
begin
  Tokenizer.Tokenize(Source, Separators, SingleLineCommentMarkers,
    MultiLineCommentStartMarkers, MultiLineCommentEndMarkers);
  TokenIndex := 0;
end{procedure};

constructor TadsParser.Create;
begin
  FBlockBegin := '{';
  FBlockEnd := '}';
  Tokenizer := TadsTokenizer.Create;
end{constructor};

destructor TadsParser.Destroy;
begin
  Tokenizer.Free;
  inherited;
end{destructor};

end.
