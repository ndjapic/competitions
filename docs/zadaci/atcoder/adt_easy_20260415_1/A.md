# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, p1, x, mx, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(p1);
	ans := 0;

	mx := -1;
	for i := 2 to n do begin
		read(x);
		mx := max(mx, x);
	end;
	readln;

	ans := max(p1, mx+1) - p1;
	writeln(ans);
end.

```
