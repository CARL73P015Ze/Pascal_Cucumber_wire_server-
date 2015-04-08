unit CucumberScenario;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
 ECucumberTestException = class(Exception)
 end;

TAction = procedure(const strList: TStringList) of object;

TLookupAction = class
  public
    ActionRegx: string;
    Action: TAction;
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
private
  valPositions: TList;
end;

 TWorld = class
 public
   FSteps: TList;
   FActions: TList;
  private
  procedure RegisterAction(strRegex: string; action: TAction);
 public
   constructor Create();
   destructor Destroy(); override;
   procedure ExecuteStep(strToMatch, actionRegx: string; callback: TAction);
   function FindStepMatches(strNameToMatch: string): TStepMatches;
   class function Get(): TWorld;
 end;

 TStepDefinition = class
 protected
   procedure CheckEquals(dLHS, dRHS: double); overload;
   procedure CheckEquals(dLHS, dRHS: Integer); overload;
   procedure CheckEquals(dLHS, dRHS: String); overload;
   procedure RegisterGiven(strRegex: string; action: TAction);
 	 procedure RegisterWhen(strRegex: string; action: TAction);
   procedure RegisterThen(strRegex: string; action: TAction);
 public
   procedure RegisterSteps(); virtual;
   procedure OnBeforeStartScenario(); virtual;
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

procedure TStepDefinition.OnBeforeStartScenario();
begin
end;

procedure TStepDefinition.OnAfterEndScenario();
begin
end;

procedure TStepDefinition.RegisterGiven(strRegex: string; action: TAction);
begin
  TWorld.Get().RegisterAction(strRegex, action);
end;

procedure TStepDefinition.RegisterWhen(strRegex: string; action: TAction);
begin
  TWorld.Get().RegisterAction(strRegex, action);
end;

procedure TStepDefinition.RegisterThen(strRegex: string; action: TAction);
begin
  TWorld.Get().RegisterAction(strRegex, action);
end;

procedure TStepDefinition.RegisterSteps();
begin

end;

constructor TStepDefinition.Scenario();
begin
  RegisterSteps();
end;

constructor TStepMatches.Create();
begin
  valPositions := TList.Create();
end;

destructor TStepMatches.Destroy();
begin
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
begin
  FActions.Free();
  FSteps.Free();
end;

class function TWorld.Get(): TWorld;
begin
  if(__World = nil) then
   __World := TWorld.Create();
  Result := __World;
end;

procedure TWorld.ExecuteStep(strToMatch, actionRegx: string; callback: TAction);
var r: TRegExpr;
  strList: TStringList;
  i: Integer;
begin
  strList := TStringList.Create();
  r :=TRegExpr.Create();
  try
    r.Expression := actionRegx;
    if(r.Exec(strToMatch)) then
    begin
      i := 0;
      while(i< r.SubExprMatchCount) do
      begin
        strList.Add(r.Match[i]);
        i:= i + 1;
      end;
      if(Assigned(callback)) then
        callback(strList);
    end;
  finally
    strList.Free();
    r.Free();
  end;
end;

procedure TWorld.RegisterAction(strRegex: string; action: TAction);
var lookup: TLookupAction;
begin
  lookup := TLookupAction.Create();
  lookup.ActionRegx:= strRegex;
  lookup.Action:= action;
  FActions.Add(lookup);
end;

function getCharPosition(text: String; bytePos: Integer): Integer;
begin
    Result := length( Copy(text, 0, bytePos) );
end;

function TWorld.FindStepMatches(strNameToMatch: string): TStepMatches;
  var iLoop: Integer;
    step: TAction;
    imatch: Integer;
    valPos: TValPos;
    PrevPos : PtrInt;
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

    PrevPos := 1;
    if regex.Exec(strNameToMatch) then
    begin
      iSubExp := 1;
      Result := TStepMatches.Create();
      Result.Id:= IntToStr(iLoop);
      Result.RegEx := TLookupAction(FActions[iLoop]).ActionRegx;
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
    iLoop := iLoop + 1;
  end;
end;


end.

