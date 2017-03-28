unit adsStringMethodsTestCases;

interface

uses
  TestFrameWork, adsStringMethods;

type
  adsStringMethodsTestCase = class(TTestCase)
  published
    procedure TestLeftPart;
    procedure TestRightPart;
    procedure TestMidPart;
    procedure TestCharCount;
    procedure TestExtendedPos;
    procedure TestSplitString;
  end{class};

implementation

uses
  Classes;

procedure adsStringMethodsTestCase.TestLeftPart;
begin
  Check(LeftPart('Hi there, Magnus!', ',') = 'Hi there');
end{procedure};

procedure adsStringMethodsTestCase.TestRightPart;
begin
  Check(RightPart('Hi there, Magnus!', ',') = ' Magnus!');
end{procedure};

procedure adsStringMethodsTestCase.TestMidPart;
begin
  Check(MidPart('Hi there!', 'i', 'r') = ' the');
end{procedure};

procedure adsStringMethodsTestCase.TestCharCount;
begin
  Check(CharCount('We are all the winners', 'e') = 4);
end{procedure};

procedure adsStringMethodsTestCase.TestExtendedPos;
begin
  Check(ExtendedPos('We are all the winners', 'e', 1) = 2);
  Check(ExtendedPos('We are all the winners', 'e', 3) = 14);
  Check(ExtendedPos('We are all the winners', 'e', 7) = -1);
end{procedure};

procedure adsStringMethodsTestCase.TestSplitString;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SplitString('"Hi there" she said', SL);
    Check(SL.Count=3);
    Check(SL[0] = '"Hi there"');
    Check(SL[1] = 'she');
    Check(SL[2] = 'said');
  finally
    SL.Free;
  end{finally};
end{procedure};

initialization
  TestFramework.RegisterTest(adsStringMethodsTestCase.Suite);

end.
