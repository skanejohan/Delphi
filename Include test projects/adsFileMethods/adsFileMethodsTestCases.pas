unit adsFileMethodsTestCases;

interface

uses
  TestFrameWork, adsFileMethods;

type
  adsFileMethodsTestCase = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function Simplify(const Info: String): String;
    procedure CreateFile(const Dir, FileName: String);
    procedure SetupStandardDirectoriesAndFiles;
    function TestDir: String;
  published
    procedure TestTextFileToStringAndStringToTextFile;
    procedure TestDeleteDirectory;
    procedure TestGetDirectories;
    procedure TestGetFiles;
    procedure TestLocateFile;
    procedure TestRenameWithCounter;
  end{class};

implementation

uses
  Classes, Forms, SysUtils;

procedure adsFileMethodsTestCase.SetUp;
begin
  inherited;
  ForceDirectories(TestDir);
end{procedure};

procedure adsFileMethodsTestCase.TearDown;
begin
  DeleteDirectory(TestDir);
  inherited;
end{procedure};

function adsFileMethodsTestCase.Simplify(const Info: String): String;
begin
  Result := StringReplace(Info, TestDir, 'TEST\', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, ';', [rfReplaceAll]);
end{function};

procedure adsFileMethodsTestCase.CreateFile(const Dir, FileName: String);
begin
  StringToTextFile(Format('this is file "%s"', [FileName]),
    Format('%s\%s', [Dir, FileName]));
end{procedure};

procedure adsFileMethodsTestCase.SetupStandardDirectoriesAndFiles;
begin
  CreateDir(TestDir + '1');
  CreateFile(TestDir + '1', '1.txt');
  CreateFile(TestDir + '1', '1.bmp');
  CreateFile(TestDir + '1', '1.gif');
  CreateFile(TestDir + '1', '1.exe');
  CreateFile(TestDir + '1', '1.bat');
  CreateDir(TestDir + '2');
  CreateDir(TestDir + '2\1');
  CreateFile(TestDir + '2\1', '1.txt');
  CreateFile(TestDir + '2\1', '1.bmp');
  CreateFile(TestDir + '2\1', '1.gif');
  CreateFile(TestDir + '2\1', '1.exe');
  CreateFile(TestDir + '2\1', '1.bat');
  CreateFile(TestDir + '2\1', '2.txt');
  CreateFile(TestDir + '2\1', '2.bmp');
  CreateFile(TestDir + '2\1', '2.gif');
  CreateFile(TestDir + '2\1', '2.exe');
  CreateFile(TestDir + '2\1', '2.bat');
  CreateDir(TestDir + '2\2');
  CreateDir(TestDir + '3');
end{procedure};

function adsFileMethodsTestCase.TestDir: String;
begin
  Result := ExtractFilePath(Application.ExeName) + 'Test\';
end{function};

procedure adsFileMethodsTestCase.TestTextFileToStringAndStringToTextFile;
begin
  StringToTextFile('this is a test', TestDir + 'test.txt');
  Check(FileExists(TestDir + 'test.txt'), TestDir + 'test.txt not found');
  CheckEquals('this is a test', TextFileToString(TestDir + 'test.txt'));
  StringToTextFile('this is a test'#13#10, TestDir + 'test.txt');
  CheckEquals('this is a test'#13#10, TextFileToString(TestDir + 'test.txt'));
end{procedure};

procedure adsFileMethodsTestCase.TestDeleteDirectory;
begin
  SetupStandardDirectoriesAndFiles;
  Check(DirectoryExists(TestDir));
  Check(DirectoryExists(TestDir + '1'), 'Directory ' + TestDir + '1 not found');
  Check(FileExists(TestDir + '1\1.txt'), 'File ' + TestDir + '1\1.txt not found');
  Check(DeleteDirectory(TestDir), 'DeleteDirectory failed');
  Check(not DirectoryExists(TestDir), 'Directory ' + TestDir + ' found');
end{procedure};

procedure adsFileMethodsTestCase.TestGetDirectories;
var
  Directories: TStrings;

  function GetDirs(const Dir: String; Clear: Boolean = True): String;
  begin
    GetDirectories(Dir, Directories, Clear);
    Result := Simplify(Directories.Text);
  end{procedure};

begin
  Directories := TStringList.Create;
  try
    SetupStandardDirectoriesAndFiles;
    CheckEquals('TEST\2\;TEST\2\1\;TEST\2\2\;', GetDirs(TestDir + '2'));
    CheckEquals('TEST\1\;', GetDirs(TestDir + '1'));
    CheckEquals('TEST\1\;TEST\2\;TEST\2\1\;TEST\2\2\;', GetDirs(TestDir + '2', False));
    CheckEquals('TEST\;TEST\1\;TEST\2\;TEST\2\1\;TEST\2\2\;TEST\3\;', GetDirs(TestDir));
  finally
    Directories.Free;
  end{finally};
end{procedure};

procedure adsFileMethodsTestCase.TestGetFiles;
var
  Files: TStrings;

  function Get(const Directory: String; const Extensions: String = '';
    Recursive: Boolean=False; Clear: Boolean=True): String;
  begin
    GetFiles(Directory, Files, Extensions, Recursive, Clear);
    Result := Simplify(Files.Text);
  end{procedure};

begin
  Files := TStringList.Create;
  try
    SetupStandardDirectoriesAndFiles;
    CheckEquals('', Get(TestDir));
    CheckEquals('TEST\1\1.bat;TEST\1\1.bmp;TEST\1\1.exe;TEST\1\1.gif;TEST\1\1.txt;', Get(TestDir + '1'));
    CheckEquals('TEST\1\1.bat;TEST\1\1.exe;', Get(TestDir + '1', 'bat;exe;com'));
    CheckEquals('TEST\1\1.bat;TEST\2\1\1.bat;TEST\2\1\2.bat;', Get(TestDir, 'bat', True));
    CheckEquals('TEST\1\1.bat;TEST\2\1\1.bat;TEST\2\1\2.bat;TEST\1\1.bat;', Get(TestDir + '1', 'bat', False, False));
  finally
    Files.Free;
  end{finally};
end{procedure};

procedure adsFileMethodsTestCase.TestLocateFile;
var
  Files: TStrings;
  FilesShort: String;

  function Locate(const Directory, FileName: String): Boolean;
  begin
    Result := LocateFile(Directory, FileName, Files);
    FilesShort := Simplify(Files.Text);
  end{procedure};

begin
  Files := TStringList.Create;
  try
    SetupStandardDirectoriesAndFiles;
    Check(Locate(TestDir, '1.bat'));
    CheckEquals('TEST\1\1.bat;TEST\2\1\1.bat;', FilesShort);
    Check(Locate(TestDir + '1', '1.bat'));
    CheckEquals('TEST\1\1.bat;', FilesShort);
    Check(not Locate(TestDir, '3.bat'));
  finally
    Files.Free;
  end{finally};
end{procedure};

procedure adsFileMethodsTestCase.TestRenameWithCounter;
var
  Files: TStrings;
begin
  Files := TStringList.Create;
  try
    SetupStandardDirectoriesAndFiles;
    Files.Add('1.bat');
    Files.Add('1.exe');
    RenameWithCounter(TestDir + '1', 'X', 3, Files);
    GetFiles(TestDir + '1', Files);
    CheckEquals('TEST\1\1.bmp;TEST\1\1.gif;TEST\1\1.txt;TEST\1\X3.bat;TEST\1\X4.exe;', Simplify(Files.Text));
    Files.Clear;
    Files.Add('1.bat');
    Files.Add('1.exe');
    RenameWithCounter(TestDir + '2\1', '', 2, Files);
    GetFiles(TestDir + '2\1', Files);
    CheckEquals(
      'TEST\2\1\1.bmp;TEST\2\1\1.gif;TEST\2\1\1.txt;TEST\2\1\2.bat;TEST\2\1\2.bmp;' +
      'TEST\2\1\2.exe;TEST\2\1\2.gif;TEST\2\1\2.txt;TEST\2\1\3.bat;TEST\2\1\4.exe;',
      Simplify(Files.Text));
  finally
    Files.Free;
  end{finally};
end{procedure};

initialization
  TestFramework.RegisterTest(adsFileMethodsTestCase.Suite);

end.
