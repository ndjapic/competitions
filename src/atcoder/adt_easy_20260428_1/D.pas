program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 10;
var
	i: int8;
	a: array [1 .. n] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: int64): int64;
begin
	result := 0;
	while x > 0 do begin
		result := result * 10 + x mod 10;
		x := x div 10;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(a[1], a[2]);

	for i := 3 to n do a[i] := f(a[i-1] + a[i-2]);

	writeln(a[n]);
end.
