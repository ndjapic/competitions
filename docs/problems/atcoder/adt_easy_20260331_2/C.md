# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, i, x: int32;
	c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 1 to n do c[x] := 0;

	for i := 1 to 4*n-1 do begin
		read(x);
		inc(c[x]);
	end;
	readln;

	for x := 1 to n do
		if c[x] < 4 then writeln(x);
end.

```
