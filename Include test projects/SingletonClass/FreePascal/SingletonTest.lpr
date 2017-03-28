program SingletonTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, SingletonTestFrm2, SingletonTestFrm1,
  VersionManagementAccessClass, DbAccessClass, SpecificDbAccessClasses,
  SpecificVersionManagementAccessClasses, SingletonClass
  { you can add units after this };

{$IFDEF WINDOWS}{$R SingletonTest.rc}{$ENDIF}

begin
  {$I SingletonTest.lrs}
  Application.Initialize;
  Application.CreateForm(TSingletonTestForm1, SingletonTestForm1);
  Application.CreateForm(TSingletonTestForm2, SingletonTestForm2);
  Application.Run;
end.

