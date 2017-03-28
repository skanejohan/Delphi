unit adsFileMethods;

{ © 2009 Aston Design Studio }

interface

uses
  Classes, SysUtils;

type
  TFileRenamedHandler = procedure(OldName, NewName: String) of object;

{** Reads the specified text file and returns the contents as a string. Raises
 an exception if the FileName does not exist. }
function TextFileToString(const FileName: String): String;

{** Writes a string to the specified text file (clearing the file first). Raises
 an exception if there is an error. }
procedure StringToTextFile(const Text, FileName: String);

{** Deletes the given directory, also if it is not empty (in which case the
 files are deleted first). Returns True if the directory was deleted
 successfully. }
function DeleteDirectory(const Directory: String): Boolean;

{** This method puts the full names of all directories below Directory
 (including Directory itself) into Directories. If the Clear parameter is
 True (the default), Directories is cleared before adding the directories.}
procedure GetDirectories(Directory: String; Directories: TStrings;
  Clear: Boolean=True);

{** This method put the full name of all files whose extensions are included in
 the Extensions parameter into Files.
 @param Directory The directory in which to look for the files.
 @param Files The string list into which the files with correct extensions are
  added. The full name (including path) is added.
 @param Extensions A string representing a list of extensions. The extensions
   should be separated by semicolon, e.g. 'bmp;gif;jpg'. Only files with the
   given extensions are added. If this string is empty (the default), all
   extensions are accepted.
 @param Recursive If this parameter is True, the Files variable will contain
  files (with correct extensions) in Directory and all its sub-directories. If
  it is False (the default), only the files in Directory are added.
 @param Clear If this parameter is True (the default), the Files list is cleared
  before the files are added. }
procedure GetFiles(const Directory: String; Files: TStrings; const Extensions:
  String = ''; Recursive: Boolean=False; Clear: Boolean=True);

{** This method tries to locate the file given by FileName in the directory
 given by Directory or one of its subdirectories. All occurences of the file
 (including the full path) are added to Files. If at least one occurence is
 found, the function returns True, otherwise False. }
function LocateFile(Directory: String; FileName: String; Files: TStrings): Boolean;

{** This method renames the files supplied. A file's new name is constructed by
 taking BaseName and adding a counter and the extension. The counter is increased
 for each renamed file.
 @param Directory This is the directory in which the files that should be renamed
  are located.
 @param BaseName This is the first letters of the new names.
 @param Start This is an integer indicating at which number the counting should
  start. If a file with the resulting name already exists in the directory,
  the number is increased.
 @param Files This parameter is a list of the names of all files that should be
  renamed. The files should be given without extension.
 @param FileRenamedHandler If this method is not nil, it will be called once for
  each file that is renamed, informing the caller of the old and new file names.
 @Example Assume we call the method with the following parameters: Directory=
  'C:\Data', BaseName='Picture', Start="100" and Files=[a.jpg, b.jpg, c.jpg].
  Assuming the the three files really exist in the given directory and no files
  with conflicting names exist, their new names would be: a.jpg -> Picture 100.jpg,
  b.jpg -> Picture 101.jpg and c.jpg -> Picture 102.jpg. Assuming that there already
  existed files with names Picture 101.jpg and Picture 102.jpg, the result would
  be: a.jpg -> Picture 100.jpg, b.jpg -> Picture 103.jpg and c.jpg -> Picture 104.jpg. }
procedure RenameWithCounter(Directory, BaseName: String; Start: Integer;
  Files: TStrings; FileRenamedHandler: TFileRenamedHandler = nil);

implementation

function TextFileToString(const FileName: String): String;
var
  TextFile: TStringList;
begin
  Result := '';
  TextFile := nil;
  try
    TextFile := TStringList.Create;
    TextFile.LoadFromFile(FileName);
    Result := TextFile.Text;
    Result := Copy(Result, 1, Length(Result) - 3); // Strip '.'#13#10
  finally
    TextFile.Free;
  end{finally};
end{function};

procedure StringToTextFile(const Text, FileName: String);
var
  TextFile: TStringList;
begin
  TextFile := nil;
  try
    TextFile := TStringList.Create;
    TextFile.Text := Text + '.'; // To make sure it ends with '.'#13#10
    TextFile.SaveToFile(FileName);
  finally
    TextFile.Free;
  end{finally};
end{function};

function DeleteDirectory(const Directory: String): Boolean;
var
  Dir: String;
  SR: TSearchRec;
begin
  Dir := IncludeTrailingPathDelimiter(Directory);
  if DirectoryExists(Dir) then
  begin
    if FindFirst(Dir + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          if (SR.Attr and faDirectory) = faDirectory then
            DeleteDirectory(Dir + SR.Name + '\')
          else
            DeleteFile(Dir + SR.Name);
        end{if};
      until FindNext(SR) <> 0;
      FindClose(SR);
    end{if};
    Result := RemoveDir(Dir);
  end {if}
  else
    Result := False;
end; {function}

procedure GetDirectories(Directory: String; Directories: TStrings;
  Clear: Boolean=True);
var
  Found: Boolean;
  SearchRec: TSearchRec;
begin
  if Clear then
    Directories.Clear;
  Directory := IncludeTrailingPathDelimiter(Directory);
  Found := (FindFirst(Directory+'*.*', faDirectory, SearchRec) = 0);
  if Found then
    Directories.Add(Directory);
  while Found do
  begin
    if SearchRec.Name[1] <> '.' then
      GetDirectories(Directory + SearchRec.Name, Directories, False);
    Found := (FindNext(SearchRec) = 0);
  end; {while}
  FindClose(SearchRec);
end; {procedure}

procedure GetFiles(const Directory: String; Files: TStrings; const Extensions:
  String = ''; Recursive: Boolean=False; Clear: Boolean=True);

  procedure GetFilesInDir(Directory, Extensions: String; Files: TStrings);
  var
    SR: TSearchRec;
    Found: Boolean;
    Extension: String;

    function CorrectExtension(Ext: String): Boolean;
    begin
      Extensions := AnsiLowerCase(Extensions);
      if Extensions = '' then
        Result := True
      else
      begin
        Ext := Copy(Ext, 2, Length(Ext)); { Remove leading '.' }
        Result := Pos(';' + Ext + ';', Extensions) <> 0;
        if not Result then
          Result := Pos(Ext + ';', Extensions) = 1;
        if not Result then
          Result := (Pos(';' + Ext, Extensions) = Length(Extensions) - Length(Ext))
            and (Length(Extensions) <> Length(Ext)); ;
        if not Result then
          Result := Ext = Extensions; { If only one extension allowed }
      end; {else}
    end; {function}

  begin
    Directory := IncludeTrailingPathDelimiter(Directory);
    Found := (FindFirst(Directory+'*.*', faAnyFile, SR) = 0);
    while Found do
    begin
      if ((SR.Attr and faDirectory) = 0) and (SR.Name[1] <> '.') then
      begin
        Extension := AnsiLowerCase(ExtractFileExt(SR.Name));
        if CorrectExtension(Extension) then
          Files.Add(Directory + SR.Name);
      end{if};
      Found := (FindNext(SR) = 0);
    end; {while}
    FindClose(SR);
  end; {procedure}

var
  i: Integer;
  Directories: TStrings;
begin
  if Clear then
    Files.Clear;
  Directories := TStringList.Create;
  try
    if Recursive then
      GetDirectories(Directory, Directories)
    else
      Directories.Add(Directory);
    for i := 0 to Directories.Count-1 do
      GetFilesInDir(Directories[i], Extensions, Files);
  finally
    Directories.Free;
  end; {try/finally}
end; {procedure}

function LocateFile(Directory: String; FileName: String; Files: TStrings): Boolean;
var
  i: Integer;
  Extension: String;
  AllFiles: TStrings;
begin
  Files.Clear;
  Extension := ExtractFileExt(FileName);
  Extension := Copy(Extension, 2, Length(Extension)); { Remove the leading '.' }
  AllFiles := TStringList.Create;
  try
    GetFiles(Directory, AllFiles, Extension, True);
    for i := 0 to AllFiles.Count-1 do
      if CompareText(ExtractFileName(AllFiles[i]), FileName) = 0 then
        Files.Add(AllFiles[i]);
    Result := Files.Count > 0;
  finally
    AllFiles.Free;
  end; {try/finally}
end; {function}

procedure RenameWithCounter(Directory, BaseName: String; Start: Integer;
  Files: TStrings; FileRenamedHandler: TFileRenamedHandler = nil);
var
  i, Counter: Integer;
  DestName: String;
begin
  Directory := IncludeTrailingPathDelimiter(Directory);
  Counter := Start;
  for i := 0 to Files.Count-1 do
  begin
    DestName := Directory + BaseName + IntToStr(Counter) + ExtractFileExt(Files[i]);
    while FileExists(DestName) do
    begin
      Inc(Counter);
      DestName := Directory + BaseName + IntToStr(Counter) + ExtractFileExt(Files[i]);
    end; {while}
    RenameFile(Directory + Files[i], DestName);
    Inc(Counter);
    if Assigned(FileRenamedHandler) then
      FileRenamedHandler(Directory + Files[i], DestName);
  end; {for}
end; {function}

end.
