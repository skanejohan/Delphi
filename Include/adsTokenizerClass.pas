unit adsTokenizerClass;

{ © 2010 Aston Design Studio }

interface

uses
  Classes, adsObjectClass;

type
  TToken = record
    Token: String;
    Line: Integer;
  end{record};
  PToken = ^TToken;

  {** Used to create a list of tokens from a source file. }
  TadsTokenizer = class(TadsObject)
  private
    FTokens: TList; {Of PToken}
    function GetTokenCount: Integer;
    function GetTokens(Index: Integer): TToken;
  protected
    procedure Clear;
  public

    {** The number of tokens available after calling Tokenize. }
    property TokenCount: Integer read GetTokenCount;

    {** The token at given position (0..TokenCount-1) after calling Tokenize.
     For each token, the actual text as well as the original source code line
     can be retrieved. }
    property Tokens[Index: Integer]: TToken read GetTokens;

    {** Call this method to split a source file into tokens. Tokens are
     separated by whitespace (or line feeds), or one of the strings in
     Separators (the line "One.Two Three" will be seen as two tokens - "One.Two"
     and "Three" if the "." is not included in Separators. If it is, the line
     will be seen as four tokens - "One", ".", "Two" and "Three".) Any text
     on a line after one of the SingleLineCommentMarkers is ignored. Any text
     (on one or more lines) between one of the MultiLineCommentStartMarkers and
     its correspoding (same index) one in MultiLineCommentEndMarkers is also
     ignored. }
    procedure Tokenize(Source: TStrings; Separators, SingleLineCommentMarkers,
      MultiLineCommentStartMarkers, MultiLineCommentEndMarkers: Array of String);

    constructor Create;
    destructor Destroy; override;
  end{class};

implementation

uses
  SysUtils, adsStringMethods, StrUtils;

function TadsTokenizer.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end{function};

function TadsTokenizer.GetTokens(Index: Integer): TToken;
begin
  Result := PToken(FTokens[Index])^;
end{function};

procedure TadsTokenizer.Clear;
var
  i: Integer;
begin
  for i := 0 to FTokens.Count-1 do
    Dispose(PToken(FTokens[i]));
  FTokens.Clear;
end{procedure};

procedure TadsTokenizer.Tokenize(Source: TStrings; Separators,
  SingleLineCommentMarkers, MultiLineCommentStartMarkers,
  MultiLineCommentEndMarkers: Array of String);
var
  Token: PToken;
  Tokens: TStrings;
  ParsingComment: Boolean;
  i, j, MarkerAt, CommentStartsAt: Integer;
  Separator, SeparatorWithSpace: String;
  StartMarker, EndMarker, BeforeComment, AfterComment: String;
begin
  Tokens := TStringList.Create;
  try
    {Remove single-line comments}
    for i := 0 to Source.Count-1 do
    begin
      for j := 0 to Length(SingleLineCommentMarkers)-1 do
      begin
        MarkerAt := Pos(SingleLineCommentMarkers[j], Source[i]);
        if MarkerAt > 0 then
          Source[i] := Copy(Source[i], 1, MarkerAt-1);
      end{for};
    end{for};

    {Remove multi-line comments}
    ParsingComment := False;
    i := 0;
    repeat
      if not ParsingComment then
      begin

        { Locate the first "start of comment" marker }
        CommentStartsAt := MAXINT;
        for j := 0 to Length(MultiLineCommentStartMarkers)-1 do
        begin
          MarkerAt := Pos(MultiLineCommentStartMarkers[j], Source[i]);
          if (MarkerAt > 0) and (MarkerAt < CommentStartsAt) then
          begin
            CommentStartsAt := MarkerAt;
            StartMarker := MultiLineCommentStartMarkers[j];
            EndMarker := MultiLineCommentEndMarkers[j];
          end{if};
        end{for};

        if CommentStartsAt < MAXINT then
        begin
          { We have found a "start of comment" marker. Either we find the
            corresponding "end of comment" marker (in which case we remove the
            text between those markers and parse this line again, for possible
            further occurences) or we don't find it (in which case we strip off
            the rest of the line, set the "ParsingComment" flag and continue
            to the next line }
          BeforeComment := Copy(Source[i], 1, CommentStartsAt-1);
          MarkerAt := PosEx(Endmarker, Source[i], CommentStartsAt + Length(StartMarker));
          if MarkerAt > 0 then
          begin
            AfterComment := Copy(Source[i], MarkerAt+Length(EndMarker), MAXINT);
            Source[i] := BeforeComment + AfterComment;
          end{if}
          else
          begin
            Source[i] := BeforeComment;
            ParsingComment := True;
            Inc(i);
          end{else};
        end{if}
        else
          Inc(i); { No start of comment on this line; continue }
      end{if};

      if ParsingComment then
      begin
        { We are currently parsing a comment. Either we find an "end of comment"
          marker (in which case we remove the text up to and including that,
          reset the "ParsingComment" flag and parse this line again, for
          possible further occurences) or we don't find it (in which case we
          clear the line and continue to the next line }
        MarkerAt := Pos(Endmarker, Source[i]);
        if MarkerAt > 0 then
        begin
          Source[i] := Copy(Source[i], MarkerAt+Length(EndMarker), MAXINT);
          ParsingComment := False;
        end{if}
        else
        begin
          Source[i] := '';
          Inc(i);
        end{else};
      end{if};
    until i = Source.Count;

    Clear;
    for i := 0 to Source.Count-1 do
    begin

      {Insert spaces around the separators}
      for j := 0 to Length(Separators)-1 do
      begin
        Separator := Separators[j];
        SeparatorWithSpace := ' ' + Separator + ' ';
        Source[i] := StringReplace(Source[i], Separator, SeparatorWithSpace, [rfReplaceAll]);
      end{for};

      {Remove multiple spaces}
      while Pos('  ', Source[i]) > 0 do
        Source[i] := StringReplace(Source[i], '  ', ' ', [rfReplaceAll]);

      {Split the line into tokens}
      SplitString(Source[i], Tokens);

      {Add corresponding tokens to FTokens}
      for j := 0 to Tokens.Count-1 do
      begin
        New(Token);
        Token^.Token := Tokens[j];
        Token^.Line := i+1;
        FTokens.Add(Token);
      end{for};
    end{for};
  finally
    Tokens.Free;
  end{finally};
end{procedure};

constructor TadsTokenizer.Create;
begin
  FTokens := TList.Create;
end{constructor};

destructor TadsTokenizer.Destroy;
begin
  Clear;
  FTokens.Free;
  inherited;
end;

end.
