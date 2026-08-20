program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 6;
var
	x, y, i, j, c: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y);

	c := 0;
	for i := 1 to N do
		for j := 1 to N do
			if (i+j >= x) or (abs(i-j) >= y) then inc(c);

	writeln(int64(c) / (N*N) : 0 : 10);
end.
