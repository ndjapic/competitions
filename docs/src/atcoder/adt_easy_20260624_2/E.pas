program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unfinished
const
	NN = 200 * 1000;
var
	n, i, j, k, h, x, y: int32;
	c: array [1 .. NN] of int8;
	a: array [1 .. 2 * NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	h := (2 * n - k) div 2;

	for i := 1 to n do c[i] := 2;

	for j := 1 to k do begin
		read(i);
		dec(c[i]);
	end;
	readln;

	x := 0;
	for i := 1 to n do
		while c[i] > 0 do begin
			inc(x);
			a[x] := i;
			dec(c[i]);
		end;
end.
