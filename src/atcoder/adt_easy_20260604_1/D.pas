program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #euclidean #distance
const
	NN = 100;
var
	n, i, j, k: int8;
	x, y: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function d2(i, j: int8): int32;
begin
	result := sqr(x[i] - x[j]) + sqr(y[i] - y[j]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(x[i], y[i]);

	for i := 1 to n do begin
		k := 1;
		for j := 1 to n do
			if d2(i, k) < d2(i, j) then k := j;
		writeln(k);
	end;
end.
