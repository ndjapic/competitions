program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i: int8;
	h: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(h[i]);
	readln;

	i := 2;
	while (i <= n) and (h[i] <= h[1]) do inc(i);

	if i > n then i := -1;
	writeln(i)
end.
