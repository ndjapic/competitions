# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, i, j, ans: int32;
	x, y: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	for i := 1 to n do begin
		readln(x[i], y[i]);
		for j := 1 to i-1 do
			ans := max(ans, sqr(x[i]-x[j]) + sqr(y[i]-y[j]));
	end;

	writeln(sqrt(ans):0:6);
end.

```
