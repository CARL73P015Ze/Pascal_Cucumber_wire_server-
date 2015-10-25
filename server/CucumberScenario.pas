unit CucumberScenario;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
 ECucumberTestException = class(Exception)
 end;

TActionNoParams = procedure() of object;
TActionStrList = procedure(const strList: TStringList) of object;
TActionI = procedure(const iVal: Integer) of object;
TActionD = procedure(const iVal: double) of object;
TActionS = procedure(const iVal: string) of object;
TActionB = procedure(const iVal: boolean) of object;


TLookupAction = class
public
  ActionRegx: string;
  ActionNoParams: TActionNoParams;

  ActionStrList: TActionStrList;
  ActionI: TActionI;
  ActionD: TActionD;
  ActionS: TActionS;
  ActionB: TActionB;
  lineNo: Integer;
  fileName: string;
public
  constructor Create(strRegEx, fname: String; iLineNo: Integer);
end;

TValPos = class
 public
   Val: String;
   Pos: Integer;
 end;

TStepMatches = class
public
   constructor Create();
   destructor Destroy(); override;
   procedure AddValPos(item: TValPos);
   function GetValPos(i: Integer): TValPos;
   function Count(): Integer;
public
  Id: string;
  regex: string;
  lineNo: Integer;
  fileName: string;
private
  valPositions: TList;
end;

TWorld = class
public
  FSteps: TList;
  FActions: TList;
private
  procedure RegisterAction(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:Integer); overload;
  procedure RegisterAction(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:Integer); overload;
  procedure RegisterAction(strRegex: string; action: TActionI; strFileName: string; iLineNum:Integer); overload;
  procedure RegisterAction(strRegex: string; action: TActionD; strFileName: string; iLineNum:Integer); overload;
  procedure RegisterAction(strRegex: string; action: TActionS; strFileName: string; iLineNum:Integer); overload;
  procedure RegisterAction(strRegex: string; action: TActionB; strFileName: string; iLineNum:Integer); overload;
public
  constructor Create();
  destructor Destroy(); override;
  function FindStepMatches(strNameToMatch: string): TStepMatches;
  class function Get(): TWorld;
end;

 TStepDefinition = class
 protected
   procedure CheckEquals(dLHS, dRHS: double); overload;
   procedure CheckEquals(dLHS, dRHS: Integer); overload;
   procedure CheckEquals(dLHS, dRHS: String); overload;

   procedure RegisterGiven(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:String); overload;
   procedure RegisterGiven(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:String); overload;
   procedure RegisterGiven(strRegex: string; action: TActionI; strFileName: string; iLineNum:String); overload;
   procedure RegisterGiven(strRegex: string; action: TActionD; strFileName: string; iLineNum:String); overload;
   procedure RegisterGiven(strRegex: string; action: TActionS; strFileName: string; iLineNum:String); overload;
   procedure RegisterGiven(strRegex: string; action: TActionB; strFileName: string; iLineNum:String); overload;

   procedure RegisterWhen(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:String); overload;
   procedure RegisterWhen(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:String); overload;
   procedure RegisterWhen(strRegex: string; action: TActionI; strFileName: string; iLineNum:String); overload;
   procedure RegisterWhen(strRegex: string; action: TActionD; strFileName: string; iLineNum:String); overload;
   procedure RegisterWhen(strRegex: string; action: TActionS; strFileName: string; iLineNum:String); overload;
   procedure RegisterWhen(strRegex: string; action: TActionB; strFileName: string; iLineNum:String); overload;


   procedure RegisterThen(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:String); overload;
   procedure RegisterThen(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:String); overload;
   procedure RegisterThen(strRegex: string; action: TActionI; strFileName: string; iLineNum:String); overload;
   procedure RegisterThen(strRegex: string; action: TActionD; strFileName: string; iLineNum:String); overload;
   procedure RegisterThen(strRegex: string; action: TActionS; strFileName: string; iLineNum:String); overload;
   procedure RegisterThen(strRegex: string; action: TActionB; strFileName: string; iLineNum:String); overload;


 public
   procedure RegisterSteps(); virtual;
   procedure SetUp(); virtual;
   procedure OnAfterEndScenario(); virtual;
   public constructor Scenario();
 end;


implementation
uses RegExpr, TypInfo;

var __World: TWorld=nil;

procedure TStepDefinition.CheckEquals(dLHS, dRHS: double);
begin
  if(dLHS <> dRHS) then
   raise ECucumberTestException.Create( FloatToStr(dLHS) + ' = ' + FloatToStr(dRHS) );
end;

procedure TStepDefinition.CheckEquals(dLHS, dRHS: Integer);
begin
  if(dLHS <> dRHS) then
   raise ECucumberTestException.Create( IntToStr(dLHS) + ' = ' + IntToStr(dRHS) );
end;

procedure TStepDefinition.CheckEquals(dLHS, dRHS: String);
begin
  if(dLHS <> dRHS) then
   raise ECucumberTestException.Create('''' + dLHS + ''' = ''' + dRHS + '''');
end;

procedure TStepDefinition.SetUp();
begin
end;

procedure TStepDefinition.OnAfterEndScenario();
begin
end;

procedure TStepDefinition.RegisterGiven(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterGiven(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, 'TODO', 0);
end;

procedure TStepDefinition.RegisterGiven(strRegex: string; action: TActionI; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterGiven(strRegex: string; action: TActionD; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterGiven(strRegex: string; action: TActionS; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterGiven(strRegex: string; action: TActionB; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterWhen(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterWhen(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterWhen(strRegex: string; action: TActionI; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterWhen(strRegex: string; action: TActionD; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterWhen(strRegex: string; action: TActionS; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterWhen(strRegex: string; action: TActionB; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterThen(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterThen(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterThen(strRegex: string; action: TActionI; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterThen(strRegex: string; action: TActionD; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterThen(strRegex: string; action: TActionS; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterThen(strRegex: string; action: TActionB; strFileName: string; iLineNum:String);
begin
  TWorld.Get().RegisterAction(strRegex, action, strFileName, StrToInt(iLineNum));
end;

procedure TStepDefinition.RegisterSteps();
begin

end;

constructor TStepDefinition.Scenario();
begin
  TWorld.Get().FSteps.Add(Self); // so we can free it later
  RegisterSteps();
  SetUp();
end;

constructor TStepMatches.Create();
begin
  valPositions := TList.Create();
  lineNo:= 0;
  fileName:= 'Unknown';
  Id:= '';
  regex:= '';
end;

destructor TStepMatches.Destroy();
var i: Integer;
begin
  i:= 0;
  while(i < valPositions.count) do
  begin
    TValPos(valPositions[i]).Free();
    i := i + 1;
  end;
  valPositions.Free();
end;

procedure TStepMatches.AddValPos(item: TValPos);
begin
  valPositions.Add(item);
end;

function TStepMatches.GetValPos(i: Integer): TValPos;
begin
  Result := TValPos(valPositions[i]);
end;

function TStepMatches.Count(): Integer;
begin
  Result := valPositions.Count;
end;


constructor TWorld.Create();
begin
  FActions := TList.Create();
  FSteps := TList.Create();
end;

destructor TWorld.Destroy();
var iLoop: Integer;
begin
  iLoop := 0;
  while(iLoop < FActions.Count) do
  begin
    TObject(FActions[iLoop]).Free;
    iLoop := iLoop + 1;
  end;
  FActions.Free();

  iLoop := 0;
  while(iLoop < FSteps.Count) do
  begin
    TObject(FSteps[iLoop]).Free;
    iLoop := iLoop + 1;
  end;
  FSteps.Free();
end;

class function TWorld.Get(): TWorld;
begin
  if(__World = nil) then
   __World := TWorld.Create();
  Result := __World;
end;

procedure TWorld.RegisterAction(strRegex: string; action: TActionNoParams; strFileName: string; iLineNum:Integer);
var lookup: TLookupAction;
begin
  lookup := TLookupAction.Create(strRegex, strFileName, iLineNum);
  lookup.ActionNoParams:= action;
  FActions.Add(lookup);
end;

procedure TWorld.RegisterAction(strRegex: string; action: TActionStrList; strFileName: string; iLineNum:Integer);
var lookup: TLookupAction;
begin
  lookup := TLookupAction.Create(strRegex, strFileName, iLineNum);
  lookup.ActionStrList:= action;
  FActions.Add(lookup);
end;

procedure TWorld.RegisterAction(strRegex: string; action: TActionI; strFileName: string; iLineNum:Integer);
var lookup: TLookupAction;
begin
  lookup := TLookupAction.Create(strRegex, strFileName, iLineNum);
  lookup.ActionI:= action;
  FActions.Add(lookup);
end;

procedure TWorld.RegisterAction(strRegex: string; action: TActionD; strFileName: string; iLineNum:Integer);
var lookup: TLookupAction;
begin
  lookup := TLookupAction.Create(strRegex, strFileName, iLineNum);
  lookup.ActionD:= action;

  FActions.Add(lookup);
end;

procedure TWorld.RegisterAction(strRegex: string; action: TActionS; strFileName: string; iLineNum:Integer);
var lookup: TLookupAction;
begin
  lookup := TLookupAction.Create(strRegex, strFileName, iLineNum);
  lookup.ActionS:= action;
  FActions.Add(lookup);
end;

procedure TWorld.RegisterAction(strRegex: string; action: TActionB; strFileName: string; iLineNum:Integer);
var lookup: TLookupAction;
begin
  lookup := TLookupAction.Create(strRegex, strFileName, iLineNum);
  lookup.ActionB:= action;
  FActions.Add(lookup);
end;

constructor TLookupAction.Create(strRegEx, fname: String; iLineNo: Integer);
begin
  fileName:= fname;
  lineNo:= iLineNo;

  ActionRegx := strRegEx;
  ActionNoParams:= nil;

  ActionStrList:= nil;
  ActionI:= nil;
  ActionD:= nil;
  ActionS:= nil;
  ActionB:= nil;
end;


function getCharPosition(text: String; bytePos: Integer): Integer;
begin
  Result := length( Copy(text, 0, bytePos) );
end;

function TWorld.FindStepMatches(strNameToMatch: string): TStepMatches;
var iLoop: Integer;
    valPos: TValPos;
    regex: TRegExpr;
    matchPos: Integer;
    matchLen: Integer;
    iSubExp: Integer;
begin
  Result := nil;
  iLoop := 0;
  while(iLoop < FActions.Count) do
  begin
    WriteLn('going to find: ' + TLookupAction(FActions[iLoop]).ActionRegx);
    WriteLn('IN: ' + strNameToMatch);

    regex := TRegExpr.Create();
    regex.Expression := TLookupAction(FActions[iLoop]).ActionRegx;

    if regex.Exec(strNameToMatch) then
    begin
      iSubExp := 1;
      Result := TStepMatches.Create();
      Result.Id:= IntToStr(iLoop);
      Result.RegEx := TLookupAction(FActions[iLoop]).ActionRegx;
      Result.fileName:= TLookupAction(FActions[iLoop]).fileName;
      Result.lineNo:= TLookupAction(FActions[iLoop]).lineNo;
      WriteLn('fname:' + Result.fileName);
      WriteLn('lineno:' + inttostr(Result.lineNo));
      while(iSubExp <= regex.SubExprMatchCount)   do
      begin
        matchPos := regex.MatchPos[iSubExp];
        matchLen := regex.MatchLen[iSubExp];
        WriteLn('len:' + inttostr(matchLen));
        WriteLn('matchPos:' + inttostr(matchpos));

        valPos := TValPos.Create();
        valPos.Val := System.Copy (strNameToMatch, matchPos, matchLen);
        valPos.Pos := matchPos;
        Result.valPositions.Add(valPos);

        iSubExp := iSubExp + 1;
      end;
      iLoop := FActions.Count;
    end;
    regex.Free();
    iLoop := iLoop + 1;
  end;
end;


finalization

// free any registered tests.
if(__World <> nil) then
 __World.Free();

end.

