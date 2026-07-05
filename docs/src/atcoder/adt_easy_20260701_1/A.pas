program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100;
var
	n, k, i: int8;
	a, b: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(a[i]);
	readln;

	k := min(k, n);

	for i := k+1 to n do b[i-k] := a[i];
	for i := 1 to k do b[i+n-k] := 0;

	for i := 1 to n-1 do write(b[i], ' ');
	writeln(b[n]);
end.
