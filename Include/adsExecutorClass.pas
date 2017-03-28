unit adsExecutorClass;

{ © 2010 Aston Design Studio }

interface

uses
  Classes, Windows;

type
  TObjectProcedure = procedure of object;

  TadsExecutor = class
  public

    {** Executes an external application according to CommandLine. If Wait is
     False, the application is spawned and then the method returns immediately.
     If Wait is True, this method does not continue until the application has
     finished running. In this case, the OnIdle method is called approximately
     every 50 ms (if assigned). This can be used e.g. to avoid an unresponsive
     user interface. If you leave WorkingDir empty, the current directory is
     used, otherwise the specified value becomes the working directory for the
     application. }
    class function Execute(const CommandLine: String; Wait: Boolean;
      const WorkingDir: String = ''; OnIdle: TObjectProcedure=nil): Boolean;

  end{class};

implementation

class function TadsExecutor.Execute(const CommandLine: String; Wait: Boolean;
  const WorkingDir: String = ''; OnIdle: TObjectProcedure=nil): Boolean;
var
  ExitCode: Longword;
  PI: TProcessInformation;
  SI: TStartupInfo;
  WorkDir: PChar;
begin
  Result := False;
  FillChar(PI, SizeOf(TProcessInformation), 0);
  FillChar(SI, SizeOf(TStartupInfo), 0);
  SI.cb := SizeOf(TStartupInfo);
  if WorkingDir = '' then
    WorkDir := nil
  else
    WorkDir := PChar(WorkingDir);
  if CreateProcess(nil, PChar(CommandLine), nil, nil, False,
    CREATE_DEFAULT_ERROR_MODE + NORMAL_PRIORITY_CLASS, nil, WorkDir, SI, PI) then
  begin
    if Wait then
    begin
      while True do
      begin
        Sleep(50);
        if GetExitCodeProcess(PI.hProcess, ExitCode) then
        begin
          if ExitCode <> STILL_ACTIVE then
          begin
            CloseHandle(PI.hProcess);
            Result := True;
            Break;
          end{if};
        end{if}
        else
        begin
          TerminateProcess(PI.hProcess, 0);
          CloseHandle(PI.hProcess);
          Break;
        end{else};
        if Assigned(OnIdle) then
          OnIdle;
      end{while};
    end{if}
    else
    begin
      CloseHandle(PI.hProcess);
      Result := True;
    end{else};
  end{if};
end{procedure};

end.
