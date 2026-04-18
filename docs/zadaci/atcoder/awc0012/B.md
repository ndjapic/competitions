# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, t, c, d, i, w: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t, c, d);
	d := min(d, c);

	ans := 0;
	for i := 1 to n do begin
		read(w);
		if w >= t then inc(ans, d);
	end;
	readln;
	writeln(ans);
end.

```
