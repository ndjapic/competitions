program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, j: int8;
	a: array [1 .. NN, 1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

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
