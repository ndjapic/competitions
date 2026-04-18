# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, t, i, a, c: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t);
	ans := 0;

	for i := 1 to n do begin
		readln(a, c);
		inc(ans, max(t-a, 0) * c);
	end;

	writeln(ans);
end.

```
