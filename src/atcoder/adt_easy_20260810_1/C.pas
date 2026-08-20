program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000 + 1;
var
	n, i: int32;
	cost: real;
	x, y: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(x[i], y[i]);

	x[0] := 0;
	y[0] := 0;
	x[n+1] := 0;
	y[n+1] := 0;

	cost := 0.0;
	for i := 0 to n do
		cost := cost + sqrt(sqr(x[i] - x[i+1]) + sqr(y[i] - y[i+1]));

	writeln(cost:0:6);
end.
