unit CucumberProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CucumberRequest, CucumberResponse, CucumberScenario;

type
  TProcessor = class
  private
    function ProcessBeginScenario() : TResponse;
    function ProcessEndScenario(): TResponse;
    function ProcessStepMatches(request: TStepMatchesCommand): TResponse;
    function ProcessInvoke(request: TInvokeCommand): TResponse;
  public
    function Process(request: TCommand): TResponse;
  end;

implementation

function TProcessor.Process(request: TCommand): TResponse;
begin
  try
  begin
    if(request is TBeginScenarioCommand) then
      Result := ProcessBeginScenario()
    else if(request is TStepMatchesCommand) then
      Result := ProcessStepMatches(TStepMatchesCommand(request))
    else if(request is TInvokeCommand) then
      Result := ProcessInvoke(TInvokeCommand(request))
    else if(request is TEndScenarioCommand) then
      Result := ProcessEndScenario()
  end
  except
    on E: Exception do
    begin
      Result := TYikesResponse.Create();
    end;
  end
end;

function TProcessor.ProcessBeginScenario() : TResponse;
begin
  // foreach registed scenario, call onStartScenario.
  // if not in a state then send the good response
  // change state to inside scenario
  Result := TSuccessResponse.Create();
  // call setup on all TStepDefinition
end;

function TProcessor.ProcessEndScenario(): TResponse;
begin
  // foreach registed scenario, call onEndScenario.
  // free up used memory maybe?
 Result := TSuccessResponse.Create();
 // change state to inside scenario
 // call teardown on all TStepDefinition
end;

function TProcessor.ProcessStepMatches(request: TStepMatchesCommand): TResponse;
var
   valpositions: TStepMatches;
begin
  valpositions := TWorld.Get().FindStepMatches(request.NameToMatch);
  if(valpositions <> nil) then
  begin
    Result := TStepMatchesResponse.Create(valpositions);
    valpositions.Free();
  end
  else
  begin
    Result := TFailResponse.Create('Unable to find match: ' +
                  TStepMatchesCommand(request).NameToMatch); // is this right?
  end;
end;

function TProcessor.ProcessInvoke(request: TInvokeCommand): TResponse;
var
   strArgsCommands: TArgsCommands;
   iCurrentInvoke: Integer;
   action: TAction;
   lookup: TLookupAction;
begin
  strArgsCommands := request.Args;
  iCurrentInvoke := 0;
  while(iCurrentInvoke < strArgsCommands.Count) do
  begin
    try
      lookup := TLookupAction(TWorld.Get().FActions[StrToInt(request.StepId)]);

      if(Assigned(lookup.Action)) then
        lookup.Action(strArgsCommands[iCurrentInvoke])
      else if(Assigned(lookup.Action)) then
        lookup.Action1()
      else
        raise ECucumberTestException.Create('Unable to execute step');

      Result := TSuccessResponse.Create();
      Except on E: ECucumberTestException do
      begin
        Result := TFailResponse.Create(E.Message);
      end
    end;
    iCurrentInvoke := iCurrentInvoke + 1;
  end;
end;

end.

