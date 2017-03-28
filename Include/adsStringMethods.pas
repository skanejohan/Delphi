unit adsStringMethods;

{ © 2010 Aston Design Studio }

interface

uses
  Classes;

{** LeftPart returns the left part of the string S, separated by Separator.
 Example: LeftPart('Hi there, Magnus!', ',') returns 'Hi there'. }
function LeftPart(const S, Separator: string): string;

{** RightPart returns the right part of the string S, separated by Separator.
 Example: RightPart('Hi there, Magnus!', ',') returns ' Magnus!'. }
function RightPart(const S, Separator: string): string;

{** MidPart returns the middle part of the string S, separated by
 LeftSeparator and RightSeparator. Example: MidPart('Hi there!', 'i', 'r')
 returns ' the'. }
function MidPart(const S, LeftSeparator, RightSeparator: String): String;

{** Returns the number of occurrences of the character C in the string S.}
function CharCount(const S: string; C: Char): Integer;

{** ExtendedPos returns the position of the n'th occurrence of the string
    given by SubStr in the string given by Str. If there are less than n
    occurences of SubStr, the method returns -1. Examples:
    ExtendedPos('We are all the winners', 'e', 1) returns 2.
    ExtendedPos('We are all the winners', 'e', 3) returns 14.
    ExtendedPos('We are all the winners', 'e', 7) returns -1.}
function ExtendedPos(Str, SubStr: String; n: Integer): Integer;

{** Puts the substrings of S separated by Delimiter characters into the Strings
 object.}
procedure DelimitedTextToStrings(const S: string; Strings: TStrings;
  Delimiter: Char);

{** Splits a string into substrings, where the substrings represent parts of the
 original string separated by ' '. If a part of the original string is contained
 in quotation marks, the part within those marks will not be split up.
 @example Calling the method with AText='"Hi there" she said' will result in
  AString containing the following strings: '"Hi there"', 'she' and 'said'
 @param AText The text to split up.
 @param AStrings The TStrings object into which the resulting strings are put.
  This object must be created before calling this method.
 @returns True if the string could be split, false otherwise (because an odd
  number of quotation marks was encountered). }
function SplitString(const AText: String; const AStrings: TStrings): Boolean;

{** Converts a string to a floating point number, using '.' as decimal
 separator, regardless of the system's default decimal separator. }
function StrToFloatDot(const S: string): Extended;

implementation

uses
  SysUtils;

function LeftPart(const S, Separator: string): string;
begin
  Result := Copy(S, 1, Pos(Separator, S)-1);
end{function};

function RightPart(const S, Separator: string): string;
begin
  if Pos(Separator, S) = 0 then
    Result := ''
  else
    Result := Copy(S, Pos(Separator, S)+Length(Separator), Length(S));
end{function};

function MidPart(const S, LeftSeparator, RightSeparator: String): String;
begin
  Result := RightPart(S, LeftSeparator);
  Result := LeftPart(Result, RightSeparator);
end{function};

function CharCount(const S: string; C: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      Inc(Result);
end{function};

function ExtendedPos(Str, SubStr: String; n: Integer): Integer;
var
  i, position: Integer;
begin
  if n < 1 then
    Result := -1
  else
  begin
    Position := 0;
    for i := 0 to n-2 do
    begin
      if Pos(SubStr, Str) = 0 then
      begin
        Str := '';
        Break;
      end; {if}
      Position := Position + Length(LeftPart(Str, Substr)) + Length(SubStr);
      Str := RightPart(Str, SubStr);
    end; {for}
    if Pos(SubStr, Str) = 0 then
      Result := -1
    else
      Result := Position + Pos(SubStr, Str);
  end; {else}
end{function};

procedure DelimitedTextToStrings(const S: string; Strings: TStrings;
  Delimiter: Char);
var
  SubStrFirstPos, DelimiterPos, SLength: Integer;

  procedure NextDelimiterPos;
  begin
    repeat Inc(DelimiterPos)
    until (DelimiterPos > SLength) or (S[DelimiterPos] = Delimiter);
  end {procedure};

begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    SubStrFirstPos := 1;
    DelimiterPos := 0;
    SLength := Length(S);
    if SLength > 0 then
      repeat
        NextDelimiterPos;
        Strings.Add(Copy(S, SubStrFirstPos, DelimiterPos - SubStrFirstPos));
        SubStrFirstPos := DelimiterPos + 1;
      until DelimiterPos > SLength;
  finally
    Strings.EndUpdate;
  end {finally};
end {procedure};

function SplitString(const AText: String; const AStrings: TStrings): Boolean;
var
  S, Part: String;
  i, QuoteCtr: Integer;
  TempStrings: TStringList;
begin
  TempStrings := TStringList.Create;
  try
    { Start by splitting the string into a number of strings where each string
      represents text outside of or inside of quotes. Example:
      '"Hi there", she said' will be split into ['"Hi there"', ', she said'] }
    S := AText + ' "';
    Part := LeftPart(S, '"');
    S := RightPart(S, '"');
    AStrings.Clear;
    QuoteCtr := 0;
    if (Part = '') and (S <> '') then
    begin
      { Take care of the special case where the first character is '"'. }
      Part := '"' + LeftPart(S, '"') + '"';
      S := RightPart(S, '"');
      Inc(QuoteCtr);
    end{if};
    Inc(QuoteCtr);
    while ((Part <> '') and (Part <> '""')) do
    begin
      if Part <> ' ' then { The last space is added by the analyser. }
        TempStrings.Add(Part);
      Part := LeftPart(S, '"');
      if (QuoteCtr mod 2) = 1 then
        Part := '"' + Part + '"';
      Inc(QuoteCtr);
      S := RightPart(S, '"');
    end{while};
    Result := QuoteCtr mod 2 = 0; { Odd number of '"' - error }

    { Now traverse all the resulting strings. Those strings who are not within
      quotes should be split up by space. After this process, the above example
      will result in ['"Hi there"', ',', 'she', 'said']. }
    if Result then
      for i := 0 to TempStrings.Count-1 do
        if TempStrings[i][1] = '"' then
          AStrings.Add(TempStrings[i])
        else
        begin
          TempStrings[i] := TrimLeft(TempStrings[i]) + ' ';
          Part := LeftPart(TempStrings[i], ' ');
          TempStrings[i] := RightPart(TempStrings[i], ' ');
          while (Part <> '') do
          begin
            AStrings.Add(Part);
            Part := LeftPart(TempStrings[i], ' ');
            TempStrings[i] := RightPart(TempStrings[i], ' ');
          end{while};
        end{else};
  finally
    TempStrings.Free;
  end{try/finally};
end{procedure};

function StrToFloatDot(const S: string): Extended;
var
  DefaultSeparator: Char;
begin
  DefaultSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Result := StrToFloat(S);
  finally
    DecimalSeparator := DefaultSeparator;
  end{finally};
end{function};

end.
