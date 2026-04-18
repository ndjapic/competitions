program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i, j: int8;
	p, q: char;
	a: array [0 .. 6] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	a[0] := 0;
	a[1] := a[0] + 3;
	a[2] := a[1] + 1;
	a[3] := a[2] + 4;
	a[4] := a[3] + 1;
	a[5] := a[4] + 5;
	a[6] := a[5] + 9;

	readln(p, q, q);

	i := ord(p) - ord('A');
	j := ord(q) - ord('A');

	writeln(abs(a[i] - a[j]));
end.
