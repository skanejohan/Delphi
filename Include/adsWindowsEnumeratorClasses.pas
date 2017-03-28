unit adsWindowsEnumeratorClasses;

{ © 2010 Aston Design Studio }

interface

uses
  Contnrs, Windows, adsObjectClass;

type

  {** Information about an active window. }
  TadsWindowInfo = class(TadsObject)
  protected
    FHandle: HWND;
    FCaption: String;
    FRect: TRect;
    constructor Create(aHandle: HWND);
  public
    property Handle: HWND read FHandle;
    property Caption: String read FCaption;
    property Rect: TRect read FRect;
  end{class};

  {** Used to retrieve information about the active windows. }
  TadsWindowsEnumerator=class(TadsObject)
  private
    FWindows: TObjectList;
    function GetWindowCount: Integer;
    function GetWindows(Index: Integer): TadsWindowInfo;
  public

    {** Returns the number of active windows, as determined by the Enumerate
     method. }
    property WindowCount: Integer read GetWindowCount;

    {** Returns the window at given index [0..WindowCount-1], as determined by
     the Enumerate method. }
    property Windows[Index: Integer]: TadsWindowInfo read GetWindows;

    {** Call this method to determine all windows currently active. }
    procedure Enumerate;

    constructor Create;
    destructor Destroy; override;
  end{class};

implementation

var
  WList: TObjectList;

function GetWindowHandle(Handle: HWND; LParam: longint): bool; stdcall;
begin
  Result := True;
  WList.Add(TadsWindowInfo.Create(Handle));
end{function};

{---------- TadsWindowInfo ----------------------------------------------------}

constructor TadsWindowInfo.Create(aHandle: HWND);
begin
  FHandle := aHandle;
end{constructor};

function TadsWindowsEnumerator.GetWindowCount: Integer;
begin
  Result := FWindows.Count;
end{function};

function TadsWindowsEnumerator.GetWindows(Index: Integer): TadsWindowInfo;
begin
  Result := FWindows[Index] as TadsWindowInfo;
end{function};

procedure TadsWindowsEnumerator.Enumerate;
var
  i: Integer;
  Buffer: array [0..255] of char;
begin
  FWindows.Clear;
  EnumWindows(@GetWindowHandle, 0);
  for i := WindowCount-1 downto 0 do
  begin
    if IsWindowVisible(Windows[i].Handle) then
    begin
      GetWindowText(Windows[i].Handle, Buffer, SizeOf(Buffer)- 1);
      if Buffer[0] <> #0 then
      begin
        Windows[i].FCaption := Buffer;
        GetWindowRect(Windows[i].Handle, Windows[i].FRect);
      end{if}
      else
        FWindows.Delete(i);
    end{if}
    else
      FWindows.Delete(i);
  end{for};
end{procedure};

constructor TadsWindowsEnumerator.Create;
begin
  FWindows := TObjectList.Create;
  WList := FWindows;
end{constructor};

destructor TadsWindowsEnumerator.Destroy;
begin
  FWindows.Free;
  inherited;
end{destructor};

end.
