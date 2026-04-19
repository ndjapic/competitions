# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, t, i, c: int32;
	ans: int64;
	h: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t);

	for i := 1 to n do read(h[i]); readln;

	ans := 0;
	for i := 1 to n do begin
		read(c);
		if h[i] <= t then inc(ans, c);
	end;
	readln;
	writeln(ans);
end.

```
