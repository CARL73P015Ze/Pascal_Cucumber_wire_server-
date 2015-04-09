program CucumberWireServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  CucumberServer,
  SampleSteps;

begin
  CucumberServer.StartServer();
end.

