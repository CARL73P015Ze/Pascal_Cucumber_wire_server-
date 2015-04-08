unit SampleSteps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CucumberScenario;

type

TCalculator = class(TStepDefinition)
private
  procedure IHaveEnteredIntoTheCalculator(const strList: TStringList);
  procedure IPressDivide(const strList: TStringList);
  procedure CalculationResult(const strList: TStringList);
private
  iCurrentNumber: double;
  iLastNumber: double;
public
  constructor Create();
  procedure RegisterSteps(); override;
end;

TConvertingCells = class(TStepDefinition)
private
  procedure IAmLoggedInAsABuyer(const strList: TStringList);
  procedure IViewWarrantyOptions(const strList: TStringList);
  procedure IShouldSeeTheFollingOptions(const strList: TStringList);
public
  constructor Create();
  procedure RegisterSteps(); override;
end;

implementation

procedure TCalculator.IHaveEnteredIntoTheCalculator(const strList: TStringList);
begin
  iLastNumber := iCurrentNumber;
  iCurrentNumber := StrToFloat(strList[0]);
end;

procedure TCalculator.IPressDivide(const strList: TStringList);
begin
  if(iCurrentNumber <> 0) then
    iCurrentNumber := iLastNumber / iCurrentNumber;
end;

procedure TCalculator.CalculationResult(const strList: TStringList);
var iLoop: Integer;
begin
  iLoop := 0;
  while(iLoop <strList.count) do
  begin
    WriteLn(strList[iLoop]);
    iLoop := iLoop + 1;
  end;

  CheckEquals(StrToFloat(strList[0]), iCurrentNumber);
end;

procedure TCalculator.RegisterSteps();
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

constructor TConvertingCells.Create();
begin

end;

procedure TConvertingCells.RegisterSteps();
begin
  RegisterGiven('^I have the company configuration:$', @IAmLoggedInAsABuyer);
  RegisterWhen('^I Click Send OnCall$', @IAmLoggedInAsABuyer);

	RegisterGiven('^I am logged in as a buyer$', @IAmLoggedInAsABuyer);
	RegisterWhen('^I view warranty options$', @IViewWarrantyOptions);
  RegisterThen('^I should see the following options:$', @IShouldSeeTheFollingOptions);


end;

constructor TCalculator.Create();
begin
  iCurrentNumber:= 0;
  iLastNumber:= 0;
end;



initialization

TConvertingCells.Scenario();
TCalculator.Scenario();
end.

