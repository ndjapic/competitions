program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, j: int8;
	a: array [1 .. nn, 1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	for i := 1 to n do begin
		for j := 1 to i do read(a[i, j]);
		readln;
	end;

	i := 1;
	for j := 1 to n do
		if i >= j then
			i := a[i, j]
		else
			i := a[j, i];

	writeln(i);
end.
