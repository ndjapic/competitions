program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, a, b, c, d, x, y: int8;
	s: int32;
	plane: array [0 .. NN, 0 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 1 to NN do
		for y := 1 to NN do
			plane[x][y] := false;

	for i := 1 to n do begin
		readln(a, b, c, d);
		for x := a+1 to b do
			for y := c+1 to d do
				plane[x][y] := true;
	end;

	s := 0;
	for x := 1 to NN do
		for y := 1 to NN do
			if plane[x][y] then inc(s);

	writeln(s);
end.
