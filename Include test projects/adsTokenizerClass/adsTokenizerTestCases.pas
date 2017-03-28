unit adsTokenizerTestCases;

interface

uses
  TestFrameWork, Classes, adsTokenizerClass;

type
  adsTokenizerTestCase = class(TTestCase)
  protected
    Source: TStrings;
    Tokenizer: TadsTokenizer;
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure ComprehensiveTest;
  end{class};

implementation

procedure adsTokenizerTestCase.Setup;
begin
  inherited;
  Source := TStringList.Create;
  Tokenizer := TadsTokenizer.Create;
end{procedure};

procedure adsTokenizerTestCase.TearDown;
begin
  inherited;
  Tokenizer.Free;
  Source.Free;
end{procedure};

procedure adsTokenizerTestCase.ComprehensiveTest;
begin
  Source.Add('  adsTokenizerTestCase = class(TTestCase ) // The class');
  Source.Add('  protected (* all this text // including this *)');
  Source.Add('    will be removed.');
  Source.Add('    Still removing.');
  Source.Add('');
  Source.Add('    until now*)Source{removed}: TStrings{also removed};');
  Source.Add('(* bla bla bla*)');
  Source.Add('    {And this is also removed}Tokenizer:TadsTokenizer;');
  Tokenizer.Tokenize(Source, ['=', '(', ')', ':', ';'], ['//'], ['(*', '{'], ['*)', '}']);
  CheckEquals(15, Tokenizer.TokenCount);
  CheckEquals('adsTokenizerTestCase', Tokenizer.Tokens[0].Token);
  CheckEquals(1, Tokenizer.Tokens[0].Line);
  CheckEquals('=', Tokenizer.Tokens[1].Token);
  CheckEquals(1, Tokenizer.Tokens[1].Line);
  CheckEquals('class', Tokenizer.Tokens[2].Token);
  CheckEquals(1, Tokenizer.Tokens[2].Line);
  CheckEquals('(', Tokenizer.Tokens[3].Token);
  CheckEquals(1, Tokenizer.Tokens[3].Line);
  CheckEquals('TTestCase', Tokenizer.Tokens[4].Token);
  CheckEquals(1, Tokenizer.Tokens[4].Line);
  CheckEquals(')', Tokenizer.Tokens[5].Token);
  CheckEquals(1, Tokenizer.Tokens[5].Line);
  CheckEquals('protected', Tokenizer.Tokens[6].Token);
  CheckEquals(2, Tokenizer.Tokens[6].Line);
  CheckEquals('Source', Tokenizer.Tokens[7].Token);
  CheckEquals(6, Tokenizer.Tokens[7].Line);
  CheckEquals(':', Tokenizer.Tokens[8].Token);
  CheckEquals(6, Tokenizer.Tokens[8].Line);
  CheckEquals('TStrings', Tokenizer.Tokens[9].Token);
  CheckEquals(6, Tokenizer.Tokens[9].Line);
  CheckEquals(';', Tokenizer.Tokens[10].Token);
  CheckEquals(6, Tokenizer.Tokens[10].Line);
  CheckEquals('Tokenizer', Tokenizer.Tokens[11].Token);
  CheckEquals(8, Tokenizer.Tokens[11].Line);
  CheckEquals(':', Tokenizer.Tokens[12].Token);
  CheckEquals(8, Tokenizer.Tokens[12].Line);
  CheckEquals('TadsTokenizer', Tokenizer.Tokens[13].Token);
  CheckEquals(8, Tokenizer.Tokens[13].Line);
  CheckEquals(';', Tokenizer.Tokens[14].Token);
  CheckEquals(8, Tokenizer.Tokens[14].Line);
end{procedure};

initialization
  TestFramework.RegisterTest(adsTokenizerTestCase.Suite);

end.
