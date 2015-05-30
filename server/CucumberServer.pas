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
  strBuffer: String;
begin
  timeout := 120000;
  aProcessor := TProcessor.Create();
  s := ASocket.RecvTerminated(Timeout, LF);
  while(s <> '') do
  begin
    WriteLn('IN: ' + s);
    try
      request := nil;
      response := nil;
      request := TCommandFactory.ParseFromJson(s);

      response := aProcessor.Process(request);
      if(response <> nil) then
      begin
        strBuffer := response.ToJsonString() + LF;
        WriteLn('OUT: ' + response.GetType() + ' ' + strBuffer);
        ASocket.SendString(strBuffer);
      end;
    finally
      request.Free();
      response.Free();
    end;
    s := ASocket.RecvTerminated(Timeout, LF);
  end;
  aProcessor.Free();
  WriteLn('<' + s);
end;

procedure StartTheServer(iPort: Integer);
var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;
  bRun: Boolean;
begin
  ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.setLinger(true,10);
  ListenerSocket.bind('0.0.0.0', IntToStr(iPort));
  ListenerSocket.listen;

  bRun := true;
  repeat
    if ListenerSocket.canread(1000) then
    begin
      ConnectionSocket.Socket := ListenerSocket.accept;
      WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
      AttendConnection(ConnectionSocket);
      ConnectionSocket.CloseSocket;
      bRun := false;
    end;

  until bRun = false;

  ListenerSocket.Free;
  ConnectionSocket.Free;
end;

procedure StartServer();
var
  iPort: Integer;
begin
  iPort := 0;
  if(Paramcount > 0) then
    iPort := StrToIntDef(ParamStr(1), 0)
  else 
    iPort := 3901;

  if((iPort > 0) and (iPort < 65535)) then
  begin
    StartTheServer(iPort);
  end
  else
  begin
    WriteLn('Invalid port');
  end;
end;


end.

