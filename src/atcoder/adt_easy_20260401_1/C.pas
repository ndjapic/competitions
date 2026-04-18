program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1000;
var
	n, i, j, c1, c2: int32;
	a, b: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]); readln;
	for i := 1 to n do read(b[i]); readln;

	c1 := 0;
	c2 := 0;
	for i := 1 to n do
		for j := 1 to n do
			if a[i] = b[j] then begin
				if i = j then
					inc(c1)
				else
					inc(c2);
			end;

	writeln(c1);
	writeln(c2);
end.
