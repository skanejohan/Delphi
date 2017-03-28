unit adsFilteredDirectoryListBoxComp;

{ © 2002-2010 Aston Design Studio }

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, SysUtils, FileCtrl;

type
  TadsFilteredDirectoryListBox = class(TDirectoryListBox)
  protected
    FAllowedDirectories: TStringList;
    procedure BuildList; override;
  public

    {** Adds the given directory to the directory list view. All directories
     above this are automatically added, and if the IncludeSubDirectories
     parameter is True, all the directory's subdirectories are also added.
     Default is False. }
    procedure AddDirectory(Directory: String; IncludeSubDirectories: Boolean=False);

    {** Removes the given directory, and all its subdirectories from the
     tree view. }
    procedure RemoveDirectory(Directory: String);

    {** Save the current allowed directories to the given file. }
    procedure SaveToFile(FileName: String);

    {** Load the allowed directories from the given file. }
    procedure LoadFromFile(FileName: String);

    {** Call this method to reflect changes made to the AllowedDirectories list. }
    procedure UpdateDirectories;

    {** Access to the string list containing all allowed (included) directories.
     The recommended way to add and remove directories is to use the AddDirectory
     and RemoveDirectory methods, but manipulating this list is also possible.
     If you make changes to this list, you must call the UpdateDirectories method
     for the changes to be visible in the component. }
    property AllowedDirectories: TStringList read FAllowedDirectories;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end{class};

implementation

uses
  adsStringMethods;

procedure TadsFilteredDirectoryListBox.BuildList;
var
  i, Index: Integer;
begin
  Items.BeginUpdate;
  try
    inherited;
    for i := Items.Count-1 downto 0 do
      if not FAllowedDirectories.Find(GetItemPath(i), Index) then
        Items.Delete(i);
  finally
    Items.EndUpdate;
  end{try/finally};
end{procedure};

procedure TadsFilteredDirectoryListBox.AddDirectory(Directory: String;
  IncludeSubDirectories: Boolean=False);

    procedure AddSubDirectories(DirToAdd: String);
    var
      Found: Boolean;
      SearchRec: TSearchRec;
    begin
      Found := (FindFirst(IncludeTrailingPathDelimiter(DirToAdd)+'*.*',
        faDirectory, SearchRec) = 0);
      while Found do
      begin
        with SearchRec do
          if ((Attr and faDirectory) <> 0) and (Name <> '.') and (Name <> '..') then
          begin
            FAllowedDirectories.Add(DirToAdd + '\' + Name);
            AddSubDirectories(DirToAdd + '\' + Name);
          end{if};
        Found := (FindNext(SearchRec) = 0);
      end{while};
    end{procedure};

var
  i: Integer;
  DirParts: TStrings;
  DirToAdd: String;
begin
  DirParts := TStringList.Create;
  try
    { Start by adding all directories down to this. }
    DelimitedTextToStrings(Directory, DirParts, '\');
    DirToAdd := DirParts[0];
    FAllowedDirectories.Add(DirToAdd + '\');
    for i := 1 to DirParts.Count-1 do
    begin
      DirToAdd := DirToAdd + '\' + DirParts[i];
      FAllowedDirectories.Add(DirToAdd);
    end{for};
    { Possibly also add the subdirectories. }
    if IncludeSubDirectories then
    begin
      DirToAdd := ExcludeTrailingPathDelimiter(DirToAdd);
      AddSubDirectories(DirToAdd);
    end{if};
    { Update the list box. }
    Update;
  finally
    DirParts.Free;
  end{try/finally};
end{procedure};

procedure TadsFilteredDirectoryListBox.RemoveDirectory(Directory: String);
var
  Index: Integer;
begin
  if FAllowedDirectories.Find(Directory, Index) then
  begin
    FAllowedDirectories.Delete(Index);
    while (Index < FAllowedDirectories.Count) and
      (Pos(Directory, FAllowedDirectories[Index]) = 1) do
      FAllowedDirectories.Delete(Index);
    Update;
  end{if};
end{procedure};

procedure TadsFilteredDirectoryListBox.SaveToFile(FileName: String);
begin
  FAllowedDirectories.SaveToFile(FileName);
end{procedure};

procedure TadsFilteredDirectoryListBox.LoadFromFile(FileName: String);
begin
  FAllowedDirectories.LoadFromFile(FileName);
  Update;
end{procedure};

procedure TadsFilteredDirectoryListBox.UpdateDirectories;
begin
  Update;
end{procedure};

constructor TadsFilteredDirectoryListBox.Create(AOwner: TComponent);
begin
  inherited;
  FAllowedDirectories := TStringList.Create;
  FAllowedDirectories.Sorted := True;
  FAllowedDirectories.Duplicates := dupIgnore;
end{constructor};

destructor TadsFilteredDirectoryListBox.Destroy;
begin
  FAllowedDirectories.Free;
  inherited;
end{destructor};

end.
