unit CucumberRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fgl, strutils;

type
  MultilineTypes = (mtNone, mtTable, mtString);

  TCommand = class
  public
  end;

  TBeginScenarioCommand = class(TCommand)
  end;

  TEndScenarioCommand = class(TCommand)
  end;

  TStepMatchesCommand = class(TCommand)
  public
    NameToMatch: string;
  end;

  TArgsCommands = specialize TFPGList<TStringList>;

  TInvokeCommand = class(TCommand)
  public
    StepId: string;
    Args: TArgsCommands; // should be an array
  end;

  TSnippetCommand = class(TCommand)
  public
    StepKeyword: string;
    StepName: string;
    Multiline: MultilineTypes; // should be an array
  end;

  TCommandFactory = class
  public
    class function ParseFromJson(json: string): TCommand;
  private
    class function CreateInvokeCommand(Data: TJSONData): TInvokeCommand;
    class function CreateStepMatchesCommand(Data: TJSONData): TStepMatchesCommand;
    class function CreateSnippetCommand(Data: TJSONData): TSnippetCommand;
  end;

implementation
uses RegExpr;

// hmm, do i need to port this??
class function TCommandFactory.CreateSnippetCommand(Data: TJSONData): TSnippetCommand;
begin
  Result := TSnippetCommand.Create();
  Result.StepKeyword := '';
  Result.StepName := '';

  if ('' = 'Cucumber::Ast::Table') then
    Result.Multiline := mtTable
  else if ('' = 'Cucumber::Ast::PyString') then
    Result.Multiline := mtString
  else
    Result.Multiline := mtNone;
end;


class function TCommandFactory.CreateInvokeCommand(Data: TJSONData): TInvokeCommand;
var iIndex, iLoop: Integer;
  someArgs: TJSONArray;
  iInnerCommand: Integer;
  strList: TStringList;
  strString: String;
begin
  Result := TInvokeCommand.Create();
  Result.Args := TArgsCommands.Create();

  iIndex := TJSONObject(Data).IndexOfName('args', False);
  if (iIndex >= 0) then
  begin
    someArgs := TJSONArray(Data.Items[iIndex]);
    if(someArgs.Count > 0) then
    begin
      if (someArgs.Items[0] is TJSONArray) then
      begin
        someArgs := TJSONArray(someArgs.Items[0]);
        iInnerCommand := 0;
        while(iInnerCommand < someArgs.Count) do
        begin
          strList := TStringList.Create();
          iLoop := 0;
          while(iLoop < TJSONArray(someArgs[iInnerCommand]).Count) do
          begin
            strString := TJSONArray(someArgs[iInnerCommand]).Items[iLoop].AsString;
            strList.Add(strString);
            iLoop := iLoop + 1;
          end;

          Result.Args.Add(strList);
          iInnerCommand := iInnerCommand + 1;
        end;
      end
      else
      begin
        strList := TStringList.Create();
        iLoop := 0;
        while(iLoop < someArgs.Count) do
        begin
          strList.Add(someArgs.Items[iLoop].AsString);
          iLoop := iLoop + 1;
        end;
        Result.Args.Add(strList);
      end;
    end
    else
    begin
      Result.Args.Add(TStringList.Create());
    end;
  end;
  iIndex := TJSONObject(Data).IndexOfName('id', False);
  if (iIndex >= 0) then
   begin
    Result.StepId := TJSONObject(Data).Items[iIndex].AsString;
  end;

end;

class function TCommandFactory.CreateStepMatchesCommand(Data: TJSONData): TStepMatchesCommand;
var
  iPos: Integer;
begin
  iPos:= TJSONObject(Data).IndexOfName('name_to_match', False);
  if (iPos >= 0) then
  begin
     Result := TStepMatchesCommand.Create();
     Result.NameToMatch := TJSONObject(Data).Items[iPos].AsString;
  end;
end;

class function TCommandFactory.ParseFromJson(json: string): TCommand;
var
  parser: TJSONParser;
  jsonData: TJSONData;
  strRequestName: String;
begin
  WriteLn(json);

  parser := TJSONParser.Create(json);
  jsonData := parser.Parse();
  if Assigned(jsonData) then
  begin
    if (jsonData.JSONType = jtArray) then
    begin
	    if(TJSONArray(jsonData).Count > 0) then
	    begin
		    strRequestName := TJSONArray(jsonData).Items[0].AsString;

		    if (strRequestName = 'begin_scenario') then
		    begin
		      Result := TBeginScenarioCommand.Create();
		    end
		    else if (strRequestName = 'step_matches') then
		    begin
		      Result := TCommandFactory.CreateStepMatchesCommand(TJSONArray(jsonData).Items[1]);
		    end
		    else if (strRequestName = 'invoke') then
		    begin
		      Result := TCommandFactory.CreateInvokeCommand(TJSONArray(jsonData).Items[1]);
		    end
		    else if (strRequestName = 'end_scenario') then
		    begin
		      Result := TEndScenarioCommand.Create();
		    end
		    else if (strRequestName = 'snippet_text') then
		    begin
		      Result := TCommandFactory.CreateSnippetCommand(TJSONArray(jsonData).Items[1]);
		    end
		    else
          raise Exception.Create('OH DEAR!!!');
	      end
	  end;
  end;
end;

end.
