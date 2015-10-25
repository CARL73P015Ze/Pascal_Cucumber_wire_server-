unit CucumberResponse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, CucumberScenario;

type
  TResponse = class
  public
    function ToJsonString(): string; virtual;
    function GetType(): string; virtual;
  end;

  TSuccessResponse = class(TResponse)
  public
    function ToJsonString(): string; override;
    function GetType(): string; override;
  end;

  TStepMatchesResponse = class(TResponse)
  private
    FvalPositions: TStepMatches;
  public
    constructor Create(valPositions: TStepMatches);
    destructor Destroy(); override;
    function ToJsonString(): string; override;
    function GetType(): string; override;
  end;

  TYikesResponse = class(TResponse)
  public
    function ToJsonString(): string; override;
    function GetType(): string; override;
  end;

  TFailResponse = class(TResponse)
  private
    FMessage: string;
  public
    constructor Create(strMessage: string);
    function ToJsonString(): string; override;
    function GetType(): string; override;
  end;

  TPendingResponse = class(TResponse)
  private
    FMessage: string;
  public
    constructor Create(strMessage: string);
    function ToJsonString(): string; override;
    function GetType(): string; override;
  end;

  TSnippetResponse = class(TSuccessResponse)
  private
    FMessage: string;
  public
    function ToJsonString(): string; override;
    function GetType(): string; override;
  end;

  TStepFailedResponse = class(TFailResponse)
  public
    function ToJsonString(): string; override;
    function GetType(): string; override;
  end;

implementation

constructor TStepMatchesResponse.Create(valPositions: TStepMatches);
var iLoop: Integer;
  valPos: TValPos;
  currentValPos: TValPos;
begin
  FvalPositions := TStepMatches.Create();
  FvalPositions.Id := valPositions.Id;
  FvalPositions.regex := valPositions.regex;
  FvalPositions.fileName:= valPositions.fileName;
  FvalPositions.lineNo:= valPositions.lineNo;

  iLoop := 0;
  while(iLoop < valPositions.Count()) do
  begin
    valPos := TValPos.Create();
    currentValPos := valPositions.GetValPos(iLoop);
    valPos.Pos:= currentValPos.Pos;
    valPos.Val:= currentValPos.Val;
    FvalPositions.AddValPos(valPos);
    iLoop:= iLoop + 1;
  end;
end;

destructor TStepMatchesResponse.Destroy();
begin
  FvalPositions.Free();
end;

function TStepMatchesResponse.ToJsonString(): string;
var jsonData: TJSONArray;
  args: TJSONArray;
  iArg: Integer;
  param: TJSONArray;
begin
  args := TJSONArray.Create();
  iArg := 0;
  while(iArg < FvalPositions.Count()) do
  begin
    args.Add(TJSONObject.Create(['val',FvalPositions.GetValPos(iArg).Val,
                                'pos',FvalPositions.GetValPos(iArg).Pos-1]));
    iArg := iArg + 1;
  end;

  param :=  TJSONArray.Create();
  param.Add(TJSONObject.Create(['regexp', FvalPositions.RegEx,
                               'id', FvalPositions.id,
                              'source', FvalPositions.fileName + ':'
                                        +IntToStr(FvalPositions.lineNo),
                              'args', args]));

  jsonData := TJSONArray.Create(['success', param]);
  Result := jsonData.FormatJSON([foSingleLineArray, foSingleLineObject], DefaultIndentSize);
  jsonData.Free();
end;

function TStepMatchesResponse.GetType(): string;
begin
  Result := 'Step Matches';
end;

function TResponse.ToJsonString(): string;
begin
  Result := '';
end;

function TResponse.GetType(): string;
begin
  Result := '';
end;

function TSuccessResponse.ToJsonString(): string;
var jsonData: TJSONArray;
begin
 jsonData := TJSONArray.Create(['success']);
 Result := jsonData.FormatJSON([foSingleLineArray, foSingleLineObject], DefaultIndentSize);
 jsonData.Free();
end;

function TSuccessResponse.GetType(): string;
begin
  Result := 'Success';
end;

function TYikesResponse.ToJsonString(): string;
begin
  Result := 'yikes';
end;

function TYikesResponse.GetType(): string;
begin
  Result := 'Yikes';
end;

constructor TFailResponse.Create(strMessage: string);
begin
  FMessage := strMessage;
end;

function TFailResponse.GetType(): string;
begin
  Result := 'Fail';
end;

function TFailResponse.ToJsonString(): string;
var jsonData: TJSONArray;
begin
  jsonData := TJSONArray.Create(['fail', TJSONObject.Create(['message', FMessage])]);
  Result := jsonData.FormatJSON([foSingleLineArray, foSingleLineObject], DefaultIndentSize);
  jsonData.Free();
end;

constructor TPendingResponse.Create(strMessage: string);
begin
  FMessage := strMessage;
end;

function TPendingResponse.ToJsonString(): string;
begin
  Result := 'fail' + FMessage;
end;

function TPendingResponse.GetType(): string;
begin
  Result := 'Pending';
end;

function TSnippetResponse.ToJsonString(): string;
begin
  Result := 'success' + FMessage;
end;

function TSnippetResponse.GetType(): string;
begin
  Result := 'Snippit';
end;


function TStepFailedResponse.ToJsonString(): string;
begin
  Result := 'fail' + 'message' + FMessage + 'exception' + 'backtrace';
end;

function TStepFailedResponse.GetType(): string;
begin
  Result := 'Step Fail';
end;

end.
