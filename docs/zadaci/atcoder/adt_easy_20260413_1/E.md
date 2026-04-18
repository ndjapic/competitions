# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, x: int32;
	ans: int64;
	c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for x := 1 to n do c[x] := 0;

	ans := 0;
	for i := 1 to n do begin
		read(x);
		if i+x <= n then inc(c[i+x]);
		if i-x >= 1 then inc(ans, c[i-x]);
	end;
	readln;

	writeln(ans);
end.

```
