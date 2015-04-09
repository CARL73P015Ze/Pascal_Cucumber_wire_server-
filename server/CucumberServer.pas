unit CucumberServer;

{$mode objfpc}{$H+}
interface
procedure StartServer();

implementation
{$ifdef fpc}
  {$mode delphi}
{$endif}

{$apptype console}


uses
  Classes, blcksock, sockets, Synautil, SysUtils, CucumberRequest, CucumberResponse,
  TypInfo, CucumberProcessor;



procedure AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout: integer;
  s: string;
  request: TCommand;
  response: TResponse;
  aProcessor: TProcessor;
begin
  timeout := 120000;
  aProcessor := TProcessor.Create();
  s := ASocket.RecvTerminated(Timeout, LF);
  while(s <> '') do
  begin
    WriteLn('IN: ' + s);
    request := TCommandFactory.ParseFromJson(s);
    response := aProcessor.Process(request);
    if(response <> nil) then
    begin
      WriteLn('OUT: ' + response.GetType() + ' ' + response.ToJsonString());
      ASocket.SendString(response.ToJsonString() + LF);
      response.Free();
    end;
    request.Free();
    s := ASocket.RecvTerminated(Timeout, LF);
  end;
  WriteLn('<' + s);
end;

procedure StartServer();

var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;
begin
  ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.setLinger(true,10);
  ListenerSocket.bind('0.0.0.0','3901');
  ListenerSocket.listen;

  repeat
    if ListenerSocket.canread(1000) then
    begin
      ConnectionSocket.Socket := ListenerSocket.accept;
      WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
      AttendConnection(ConnectionSocket);
      ConnectionSocket.CloseSocket;
    end;
  until false;

  ListenerSocket.Free;
  ConnectionSocket.Free;
end;


end.

