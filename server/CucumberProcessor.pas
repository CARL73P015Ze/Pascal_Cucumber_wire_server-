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
   iCurrentInvoke: Integer;
   lookup: TLookupAction;
   args: TStringList;
begin
  iCurrentInvoke := 0;
  try
  begin
    while(iCurrentInvoke < request.GetArgCount()) do
    begin
      lookup := TLookupAction(TWorld.Get().FActions[StrToInt(request.StepId)]);
      args:= request.GetArgStrings(iCurrentInvoke);

      if(Assigned(lookup.ActionStrList)) then
        lookup.ActionStrList(args)
      else if(Assigned(lookup.ActionNoParams)) then
        lookup.ActionNoParams()
      else if(request.GetArgCount() > 0) then
      begin
        if(Assigned(lookup.ActionI)) then
          lookup.ActionI(StrToInt(args[0]))
        else if(Assigned(lookup.ActionD)) then
          lookup.ActionD(StrToFloat(args[0]))
        else if(Assigned(lookup.ActionS)) then
          lookup.ActionS(args[0])
        else if(Assigned(lookup.ActionB)) then
          lookup.ActionB(StrToBool(args[0]))
      end
      else
        raise ECucumberTestException.Create('Unable to execute step');
      iCurrentInvoke := iCurrentInvoke + 1;
    end;
    Result := TSuccessResponse.Create();
  end
  Except on E: ECucumberTestException do
  begin
    Result := TFailResponse.Create(E.Message);
  end;
end;
end;

end.

