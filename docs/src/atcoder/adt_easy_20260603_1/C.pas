program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	HH = 3;
	WW = 5;
	NN = 90;
var
	h, w, n, i, j, k, x, ans: int8;
	row: array [0 .. NN] of int8;
	c: array [1 .. HH] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, n);

	for x := 1 to NN do row[x] := 0;
	for i := 0 to H do c[i] := 0;

	for i := 1 to h do begin
		for j := 1 to w do begin
			read(x);
			row[x] := i;
		end;
		readln;
	end;

	for k := 1 to n do begin
		readln(x);
		i := row[x];
		inc(c[i]);
	end;

	ans := 0;
	for i := 1 to h do ans := max(ans, c[i]);

	writeln(ans);
end.
