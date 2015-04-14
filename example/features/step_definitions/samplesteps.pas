unit SampleSteps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CucumberScenario;

type

TCalculator = class
private
  FCurrentNumber: double;
  FLastNumber: double;
public
  procedure EnterNumber(value: double);
  procedure Divide();
  function GetCurrentValue(): double;
  constructor Create();
end;

TCalculatorSteps = class(TStepDefinition)
private
  procedure IHaveEnteredIntoTheCalculator(const dNumber: Double);
  procedure IPressDivide();
  procedure CalculationResult(const dNumber: Double);
private
  FCalculator: TCalculator;
public
  procedure SetUp(); override;
  procedure RegisterSteps(); override;
end;

TConvertingCells = class(TStepDefinition)
private
  procedure IAmLoggedInAsABuyer();
  procedure IViewWarrantyOptions();
  procedure IShouldSeeTheFollingOptions(const strList: TStringList);
public
  procedure RegisterSteps(); override;
end;

TSendingAnItemToCarl = class(TStepDefinition)
private
  procedure IHaveTheCompanyConfiguration(const strList: TStringList);
  procedure IClickSend();
  procedure ReveivesTheMessage();
public
  procedure RegisterSteps(); override;
end;


implementation

constructor TCalculator.Create();
begin
  FCurrentNumber := 0;
  FLastNumber:= 0;
end;

procedure TCalculator.EnterNumber(value: double);
begin
  FLastNumber:= FCurrentNumber;
  FCurrentNumber := value;
end;

procedure TCalculator.Divide();
begin
  if(FCurrentNumber <> 0) then
    FCurrentNumber := FLastNumber / FCurrentNumber
  else
    FCurrentNumber:= 0;
end;

function TCalculator.GetCurrentValue(): double;
begin
  Result := FCurrentNumber;
end;

procedure TCalculatorSteps.SetUp();
begin
  FCalculator := TCalculator.Create();
end;

procedure TCalculatorSteps.IHaveEnteredIntoTheCalculator(const dNumber: Double);
begin
  FCalculator.EnterNumber(dNumber);
end;

procedure TCalculatorSteps.IPressDivide();
begin
  FCalculator.Divide();
end;

procedure TCalculatorSteps.CalculationResult(const dNumber: Double);
var value: double;
begin
  value:= FCalculator.GetCurrentValue();
  CheckEquals(dNumber, value);
end;

procedure TCalculatorSteps.RegisterSteps();
begin
	RegisterGiven('^I have entered (\d+) into the calculator$', @IHaveEnteredIntoTheCalculator);
	RegisterWhen('^I press divide$', @IPressDivide);
  RegisterThen('^the result should be ([-+]?([0-9]*\.[0-9]+|[0-9]+)) on the screen$', @CalculationResult);
end;

procedure TConvertingCells.IAmLoggedInAsABuyer();
begin

end;

procedure TConvertingCells.IViewWarrantyOptions();
begin

end;

procedure TConvertingCells.IShouldSeeTheFollingOptions(const strList: TStringList);
var iLoop: Integer;
begin
  iLoop := 0;
  while(iLoop <strList.count) do
  begin
    WriteLn(strList[iLoop]);
    iLoop := iLoop + 1;
  end;
end;

procedure TConvertingCells.RegisterSteps();
begin
	RegisterGiven('^I am logged in as a buyer$', @IAmLoggedInAsABuyer);
	RegisterWhen('^I view warranty options$', @IViewWarrantyOptions);
  RegisterThen('^I should see the following options:$', @IShouldSeeTheFollingOptions);
end;




procedure TSendingAnItemToCarl.IHaveTheCompanyConfiguration(const strList: TStringList);
begin

end;

procedure TSendingAnItemToCarl.IClickSend();
begin

end;

procedure TSendingAnItemToCarl.ReveivesTheMessage();
begin

end;

procedure TSendingAnItemToCarl.RegisterSteps();
begin
  RegisterGiven('^I have the company configuration:$', @IHaveTheCompanyConfiguration);
  RegisterWhen('^I Click Send$', @IClickSend);
  RegisterWhen('^([a-zA-Z]+) receives the message$', @ReveivesTheMessage);
end;

initialization

TConvertingCells.Scenario();
TCalculatorSteps.Scenario();
TSendingAnItemToCarl.Scenario();
end.

