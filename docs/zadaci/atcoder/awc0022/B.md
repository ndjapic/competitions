# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	mm = 200 * 1000;
var
	n, m, i, ans: int32;
	t, a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, t);

	ans := 0;
	for i := 1 to n do begin
		read(a);
		inc(ans, max(0, t-a));
	end;
	readln;

	if ans > m then ans := -1;
	writeln(ans);
end.

```
