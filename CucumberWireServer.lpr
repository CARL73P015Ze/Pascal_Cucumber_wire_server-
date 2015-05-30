program CucumberWireServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  CucumberServer,
  SampleSteps,
  sysutils;


begin

  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
 {
  if FileExists('D:\Development\Pascal_Cucumber_wire_server-\heap.trc') then
    DeleteFile('D:\Development\Pascal_Cucumber_wire_server-\heap.trc');
  SetHeapTraceOutput('D:\Development\Pascal_Cucumber_wire_server-\heap.trc');
 }

  CucumberServer.StartServer();
end.

