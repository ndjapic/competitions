program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100;
var
	n, l, r: int8;
	ans: int32;
	x, y: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	for r := 1 to n do begin
		readln(x[r], y[r]);
		for l := 1 to r-1 do
			ans := max(ans, sqr(x[r] - x[l]) + sqr(y[r] - y[l]));
	end;

	writeln(sqrt(ans):0:6);
end.
