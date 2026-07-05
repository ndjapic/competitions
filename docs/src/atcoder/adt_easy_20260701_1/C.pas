program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, j, d: int8;
	x: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	x[1] := 0;
	for i := 1 to n-1 do begin
		read(d);
		x[i+1] := x[i] + d;
	end;
	readln;

	for i := 1 to n-1 do begin
		for j := 1 to n-i-1 do write(x[i+j] - x[i], ' ');
		writeln(x[n] - x[i], ' ');
	end;
end.
