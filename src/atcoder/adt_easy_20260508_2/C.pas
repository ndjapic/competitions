program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, x, y, z: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 0 to n do
		for y := 0 to n-x do
			for z := 0 to n-x-y do
				writeln(x, ' ', y, ' ', z);
end.
