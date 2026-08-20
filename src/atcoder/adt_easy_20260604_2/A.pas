program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j: int8;
	ans: int32;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(a[i]);
	readln;

	ans := 0;
	for j := 1 to m do begin
		read(i);
		inc(ans, a[i]);
	end;
	readln;
	writeln(ans);
end.
