# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i: int32;
	ans: int64;
	a, b: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do read(a[i]); readln;
	for i := 1 to n do read(b[i]); readln;

	ans := 0;
	for i := 1 to n do
		inc(ans, abs(a[i] - b[i]));

	writeln(ans);
end.

```
