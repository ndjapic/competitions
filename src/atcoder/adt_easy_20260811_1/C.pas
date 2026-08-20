program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, a, b, c, d, x, y: int8;
	ans: int32;
	region: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 1 to NN do begin
		setlength(region[x], NN);
		for y := 1 to NN do region[x][y] := '.';
	end;

	for i := 1 to n do begin
		readln(a, b, c, d);
		for x := a+1 to b do
			for y := c+1 to d do
				region[x][y] := '#';
	end;

	ans := 0;
	for x := 1 to NN do
		for y := 1 to NN do
			if region[x][y] = '#' then inc(ans);

	writeln(ans);
end.
