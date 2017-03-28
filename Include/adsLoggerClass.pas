unit adsLoggerClass;

{ © 2009-2010 Aston Design Studio }

{ Compiler directives: NOAUTOCREATELOG, NOLOGGING, INSANELOGGING }

interface

uses
  Classes, SyncObjs, Windows, SysUtils;

type

  {** Classification of log information. ltError is intended for use when an
   error has occurred (i.e. this log information probably causes a bug fix).
   ltWarning should be used when an event occurs that may occur without there
   being an error in the application, but that may be the result e.g. of an
   invalid configuration. ltInformation should be used if you know that you will
   want to look in the log for some specific information later, possibly even
   though there is no problem with the system. ltDebug should be used for
   information that may be needed to help track down a problem, but that may
   possibly clutter a file. ltInsane should be used for debug information that
   you will normally not generate at all since it may slow down the application
   (see the comment for LogInsane below). ltMemDebug is used by TadsObject to
   log creation and destruction of objects. ltMemError is used by TadsObject to
   log memory leaks. }
  TadsLogType = (ltError, ltWarning, ltInformation, ltDebug, ltInsane,
    ltMemDebug, ltMemError);
  TadsLogTypes = set of TadsLogType;

const
  LogTypeNames: Array[TadsLogType] of String = ('Error', 'Warning',
    'Information', 'Debug', 'Insane', 'MemDebug', 'MemError');

type

  {** Objects of this type are used by TadsLogger to keep information about a
   log entry. }
  TadsLog = record
    LogType: TadsLogType;
    DateTime: TDateTime;
    SenderClass: String;
    Description: String;
  end{record};
  PadsLog = ^TadsLog;

  {** Objects of this type are used by TadsLogger to keep information about a
   log subscription. }
  TadsLogSubscription = class
  protected
    LogFile: TextFile;
    FileName: String;
    FileSize: Integer;
    MaxFileSize: Integer;
    BackupFiles: Boolean;
    LogTypes: TadsLogTypes;
    procedure CreateOrAppendFile;
  public
    procedure Write(const LogEntry: String);
    constructor Create(const FileName: String; LogTypes: TadsLogTypes;
      MaxFileSize: Integer; BackupFiles: Boolean);
    destructor Destroy; override;
  end{class};

  {** TadsLogger uses one object of this type to perform the actual logging
   (i.e. writing to file) in a separate thread. }
  TadsLogThread = class(TThread)
  protected
    Logs: TList;
    NoOfWaitingLogs: Integer;
    LogSubscriptions: TList;
    LogsAccess: TCriticalSection;
    procedure Execute; override;
  public
    procedure AddLog(Log: PadsLog);
    procedure AddLogSubscription(LogSubscription: TadsLogSubscription);
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
  end{class};

  {** The class used for logging. Under normal circumstances, an instance
   (Logger) of this class is automatically created, and you use the global
   Log... methods (see below) to log to it. }
  TadsLogger = class
  protected
    LogThread: TadsLogThread;
    LoggingStarted: Boolean;
  public

    {** Call this method to add a log event subscription, i.e. to determine a
     file where some or all log events will be stored. Note that you may call
     this method several times, e.g. if you want file with only errors and
     warnings, and another one with all log information. If you call this method
     after you have commenced logging (i.e  after the first call to Log),
     nothing happens.
     @param FileName This is the name of the file onto which log information
      will be written.
     @param LogTypes This parameter indicates which log types that will be
      written to the file (also see comments in the log... methods below about
      compiler directives).
     @param MaxFileSize. Indicates the maximum file size in kB. When the log
      file becomes larger than this value it is either cleared or backup up (see
      the BackupFiles parameter) and a new file with the same name ís started.
     @param BackupFiles. If this parameter is True, a file will be backed up
      when its size limit is exceeded. The name of the backup will match that of
      the original file but with a time stamp appended (example: MyLogFile.log
      will be backed up as e.g. MyLogFile_091005_203315.log). If the parameter
      is False, the file is deleted when it becomes too large. }
    procedure AddSubscription(const FileName: String; LogTypes: TadsLogTypes =
      [ltError, ltWarning, ltInformation, ltDebug, ltInsane];
      MaxFileSize: Integer = 1024; BackupFiles: Boolean = True);

    {** Call this method to add a log entry. If you use the default Logger
     object, use the global Log... methods instead, since they will look at the
     compiler directives defined. }
    procedure Log(Sender: TObject; LogType: TadsLogType; const Description: String);

    constructor Create;
    destructor Destroy; override;
  end{class};

var
  Logger: TadsLogger;

{** Call this method to log an error to the default logger object (Logger). If
 the compiler directive NOAUTOCREATELOG is defined, the Logger object is not
 created automatically, and you may not call this method, but use the Log method
 of another TadsLogger object directly instead. If the NOLOGGING compiler
 directive is defined, this method will be empty and all calls to it will be
 removed by the compiler, thus causing absolutely no overhead. }
procedure LogError(Sender: TObject; const Description: String);

{** Call this method to log an error due to an exception. Will log "Exception
 (exception class) with message "(exception message)" raised when (when).'
 Also see comment for LogError. }
procedure LogException(Sender: TObject; E: Exception; const When: String);

{** Call this method to log a warning to the default logger object (Logger).
 Also see comment for LogError. }
procedure LogWarning(Sender: TObject; const Description: String);

{** Call this method to log information to the default logger object (Logger).
 Also see comment for LogError. }
procedure LogInformation(Sender: TObject; const Description: String);

{** Call this method to log debug information to the default logger object
 (Logger). Also see comment for LogError. }
procedure LogDebug(Sender: TObject; const Description: String);

{** Call this method to log "insane" debug information to the default logger
 object (Logger). Also see comment for LogError. "Insane" debug information
 is intended for information that will never be used i production environment,
 since it will clutter the log files too much. Note that the compiler directive
 INSANELOGGING must be set if you want "insane" log information. If not, this
 method will be empty and all calls to it will be removed by the compiler, thus
 causing absolutely no overhead. }
procedure LogInsane(Sender: TObject; const Description: String);

implementation

const
  dtFormat = 'yyyy-mm-dd hh:nn:ss.zzz';

procedure LogError(Sender: TObject; const Description: String);
begin
  {$ifndef NOLOGGING}
  Logger.Log(Sender, ltError, Description);
  {$endif}
end{procedure};

procedure LogException(Sender: TObject; E: Exception; const When: String);
begin
  {$ifndef NOLOGGING}
  Logger.Log(Sender, ltError, Format('Exception %s with message "%s" raised ' +
    'when %s.', [E.ClassName, E.Message, When]));
  {$endif}
end{procedure};

procedure LogWarning(Sender: TObject; const Description: String);
begin
  {$ifndef NOLOGGING}
  Logger.Log(Sender, ltWarning, Description);
  {$endif}
end{procedure};

procedure LogInformation(Sender: TObject; const Description: String);
begin
  {$ifndef NOLOGGING}
  Logger.Log(Sender, ltInformation, Description);
  {$endif}
end{procedure};

procedure LogDebug(Sender: TObject; const Description: String);
begin
  {$ifndef NOLOGGING}
  Logger.Log(Sender, ltDebug, Description);
  {$endif}
end{procedure};

procedure LogInsane(Sender: TObject; const Description: String);
begin
  {$ifndef NOLOGGING}
  {$ifdef INSANELOGGING}
  Logger.Log(Sender, ltInsane, Description);
  {$endif}
  {$endif}
end{procedure};

{---------- TadsLogSubscription -----------------------------------------------}

procedure TadsLogSubscription.CreateOrAppendFile;
var
  SR: TSearchRec;
begin
  AssignFile(LogFile, FileName);
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    Self.FileSize := SR.Size;
    Append(LogFile);
  end{if}
  else
  begin
    Self.FileSize := 0;
    Rewrite(LogFile);
  end{else};
  Flush(LogFile);
  FindClose(SR);
end{procedure};

procedure TadsLogSubscription.Write(const LogEntry: String);

    function BackupFileName: String;
    var
      Ext: String;
    begin
      Ext := ExtractFileExt(FileName);
      Result := Copy(FileName, 1, Length(FileName) - Length(Ext)) + '_' +
        FormatDateTime('yymmdd_hhmmss', Now) + Ext;
    end{function};

begin
  Writeln(LogFile, LogEntry);
  Flush(LogFile);
  Inc(FileSize, Length(LogEntry) + 2{CR/LF});
  if FileSize > MaxFileSize * 1024 then
  begin
    CloseFile(LogFile);
    if BackupFiles then
      RenameFile(FileName, BackupFileName)
    else
      DeleteFile(FileName);
    CreateOrAppendFile;
  end{if};
end{procedure};

constructor TadsLogSubscription.Create(const FileName: String; LogTypes: TadsLogTypes;
  MaxFileSize: Integer; BackupFiles: Boolean);
begin
  Self.FileName := FileName;
  Self.LogTypes := LogTypes;
  Self.MaxFileSize := MaxFileSize;
  Self.BackupFiles := BackupFiles;
  CreateOrAppendFile;
end{constructor};

destructor TadsLogSubscription.Destroy;
begin
  CloseFile(LogFile);
  inherited;
end{destructor};

{---------- TadsLogThread -----------------------------------------------------}

procedure TadsLogThread.Execute;
var
  Log: PadsLog;

  function GetOldestLogEntry: Boolean;
  begin
    Result := False;
    LogsAccess.Acquire;
    try
      if Logs.Count > 0 then
      begin
        Dec(NoOfWaitingLogs);
        Log := Logs[0];
        Logs.Delete(0);
        Result := True;
      end{if};
    finally
      LogsAccess.Release;
    end{finally};
  end{function};

  procedure WriteLogEntry;
  var
    i: Integer;
    S: String;
  begin
    S := FormatDateTime(dtFormat, Log^.DateTime) + ';' +
      LogTypeNames[Log^.LogType] + ';' +
      Log^.SenderClass + ';' +
      Log^.Description;
    for i := 0 to LogSubscriptions.Count-1 do
      with TadsLogSubscription(LogSubscriptions[i]) do
        if Log^.LogType in LogTypes then
          Write(S);
  end{procedure};

var
  LogEntryFound: Boolean;
begin
  while (not Terminated) or (NoOfWaitingLogs > 0) do
  begin
    LogEntryFound := GetOldestLogEntry;
    if LogEntryFound then
    begin
      WriteLogEntry;
      Dispose(Log);
    end{if};
  end{while};
end{procedure};

procedure TadsLogThread.AddLog(Log: PadsLog);
begin
  LogsAccess.Acquire;
  try
    Inc(NoOfWaitingLogs);
    Logs.Add(Log);
  finally
    LogsAccess.Release;
  end{finally};
end{procedure};

procedure TadsLogThread.AddLogSubscription(LogSubscription: TadsLogSubscription);
begin
  LogSubscriptions.Add(LogSubscription);
end{procedure};

constructor TadsLogThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  NoOfWaitingLogs := 0;
  Logs := TList.Create;
  LogSubscriptions := TList.Create;
  LogsAccess := TCriticalSection.Create;
end{constructor};

destructor TadsLogThread.Destroy;
var
  i: Integer;
begin
  for i := 0 to LogSubscriptions.Count-1 do
    TadsLogSubscription(LogSubscriptions[i]).Free;
  LogSubscriptions.Free;
  LogsAccess.Free;
  Logs.Free;
  inherited;
end{destructor};

{---------- TadsLogger --------------------------------------------------------}

procedure TadsLogger.Log(Sender: TObject; LogType: TadsLogType;
  const Description: String);
var
  Log: PadsLog;
begin
  New(Log);
  Log^.LogType := LogType;
  Log^.DateTime := Now;
  if Assigned(Sender) then
    Log^.SenderClass := Sender.ClassName
  else
    Log^.SenderClass := '<<nil>>';
  Log^.Description := Description;
  LogThread.AddLog(Log);
  LoggingStarted := True;
end{procedure};

procedure TadsLogger.AddSubscription(const FileName: String;
  LogTypes: TadsLogTypes = [ltError, ltWarning, ltInformation, ltDebug, ltInsane];
  MaxFileSize: Integer = 1024; BackupFiles: Boolean = True);
begin
  if not LoggingStarted then
    LogThread.AddLogSubscription(TadsLogSubscription.Create(FileName, LogTypes,
      MaxFileSize, BackupFiles));
end{procedure};

constructor TadsLogger.Create;
begin
  LogThread := TadsLogThread.Create(True);
  LogThread.FreeOnTerminate := False;
  LogThread.Resume;
  LoggingStarted := False;
end{constructor};

destructor TadsLogger.Destroy;
begin
  LogThread.Terminate;
  LogThread.WaitFor;
  LogThread.Free;
  inherited;
end{destructor};

{$ifndef NOAUTOCREATELOG}
initialization
  Logger := TadsLogger.Create;

finalization
  Logger.Free;
{$endif}

end.
