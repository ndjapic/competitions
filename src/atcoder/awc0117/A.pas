program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000;
var
	n, i: int32;
	ans: int64;
	a: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	ans := max(a[1] + a[2], a[n-1] + a[n]);
	for i := 2 to n-1 do ans := max(ans, a[i-1] + a[i] + a[i+1]);

	writeln(ans);
end.
