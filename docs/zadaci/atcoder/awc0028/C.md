# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, i, a, b, mx: int32;
	ans: int64;
	p: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(p[i]);
	readln;

	ans := 0;
	mx := 0;
	for i := 1 to n do begin
		read(a);
		inc(ans, a);
		mx := max(mx, p[i] - a);
	end;
	readln;

	for i := 1 to n do begin
		read(b);
		dec(ans, b);
	end;
	readln;

	writeln(ans + mx);
end.

```
