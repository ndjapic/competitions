program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, k, i: int8;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(a[i]);
	readln;

	for i := n-k+1 to n do write(a[i], ' ');
	for i := 1 to n-k-1 do write(a[i], ' ');
	writeln(a[n-k]);
end.
