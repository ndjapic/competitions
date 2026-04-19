# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 7;
var
	i: int8;
	ans: int32;
	a, b: array [1 .. n] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to n do read(a[i]); readln;
	for i := 1 to n do read(b[i]); readln;

	ans := 0;
	for i := 1 to n do inc(ans, a[i] * b[i]);

	writeln(ans);
end.

```
