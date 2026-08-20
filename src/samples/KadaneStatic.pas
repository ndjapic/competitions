program KadaneStatic;

{$MODE DELPHI}

uses
	Math;

const
	NIZ_VELICINA = 8;

type
	TStatickiNiz = array[1..NIZ_VELICINA] of Integer;

function MaxSubArraySum(const Arr: TStatickiNiz): Integer;
var
	MaxSoFar: Integer;
	MaxEndingHere: Integer;
	I: Integer;
begin
	MaxSoFar := Arr[1];
	MaxEndingHere := Arr[1];

	for I := 2 to NIZ_VELICINA do
	begin
		MaxEndingHere := Max(Arr[I], MaxEndingHere + Arr[I]);
		MaxSoFar := Max(MaxSoFar, MaxEndingHere);
	end;

	Result := MaxSoFar;
end;

var
	Numbers: TStatickiNiz;
	Ans: Integer;
begin
	Numbers[1] := -2;
	Numbers[2] := -3;
	Numbers[3] := 4;
	Numbers[4] := -1;
	Numbers[5] := -2;
	Numbers[6] := 1;
	Numbers[7] := 5;
	Numbers[8] := -3;

	Ans := MaxSubArraySum(Numbers);
	
	Writeln('Maksimalna suma podniza je: ', Ans);
	Readln;
end.
