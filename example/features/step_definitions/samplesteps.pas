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
  procedure IHaveEnteredIntoTheCalculator(const strList: TStringList);
  procedure IPressDivide();
  procedure CalculationResult(const strList: TStringList);
private
  FCalculator: TCalculator;
public
  procedure SetUp(); override;
  procedure RegisterSteps(); override;
end;

TConvertingCells = class(TStepDefinition)
private
  procedure IAmLoggedInAsABuyer(const strList: TStringList);
  procedure IViewWarrantyOptions(const strList: TStringList);
  procedure IShouldSeeTheFollingOptions(const strList: TStringList);
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

procedure TCalculatorSteps.IHaveEnteredIntoTheCalculator(const strList: TStringList);
begin
  FCalculator.EnterNumber(StrToFloat(strList[0]));
end;

procedure TCalculatorSteps.IPressDivide();
begin
  FCalculator.Divide();
end;

procedure TCalculatorSteps.CalculationResult(const strList: TStringList);
var value: double;
begin
  value:= FCalculator.GetCurrentValue();
  CheckEquals(StrToFloat(strList[0]), value);
end;

procedure TCalculatorSteps.RegisterSteps();
begin
	RegisterGiven('^I have entered (\d+) into the calculator$', @IHaveEnteredIntoTheCalculator);
	RegisterWhen('^I press divide$', @IPressDivide);
  RegisterThen('^the result should be ([-+]?([0-9]*\.[0-9]+|[0-9]+)) on the screen$', @CalculationResult);
end;

procedure TConvertingCells.IAmLoggedInAsABuyer(const strList: TStringList);
begin

end;

procedure TConvertingCells.IViewWarrantyOptions(const strList: TStringList);
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
  RegisterGiven('^I have the company configuration:$', @IAmLoggedInAsABuyer);
  RegisterWhen('^I Click Send OnCall$', @IAmLoggedInAsABuyer);

	RegisterGiven('^I am logged in as a buyer$', @IAmLoggedInAsABuyer);
	RegisterWhen('^I view warranty options$', @IViewWarrantyOptions);
  RegisterThen('^I should see the following options:$', @IShouldSeeTheFollingOptions);
end;





initialization

TConvertingCells.Scenario();
TCalculatorSteps.Scenario();
end.

