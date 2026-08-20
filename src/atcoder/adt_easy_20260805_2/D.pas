program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, x, y: int8;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	y := 1;
	for i := 1 to n do begin
		read(a[i]);
		if a[y] < a[i] then y := i;
	end;
	readln;

	x := 0;
	for i := 1 to n do
		if (i <> y) and ((x = 0) or (a[x] < a[i])) then x := i;

	writeln(x);
end.
