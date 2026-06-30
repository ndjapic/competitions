program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 2;
var
	r, c, i, j: int8;
	a: array [1 .. NN, 1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, c);

	for i := 1 to NN do begin
		for j := 1 to NN do read(a[i, j]);
		readln;
	end;

	writeln(a[r, c]);
end.
