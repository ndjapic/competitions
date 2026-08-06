program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #outer #product #triangle #area #collinear
const
	NN = 300;
var
	n, i, j, k, ans: int32;
	x, y: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	for i := 1 to n do begin
		readln(x[i], y[i]);
		for j := 2 to i-1 do
			for k := 1 to j-1 do
				if (x[i] - x[j]) * (y[j] - y[k]) <> (x[j] - x[k]) * (y[i] - y[j]) then
					inc(ans);
	end;

	writeln(ans);
end.
