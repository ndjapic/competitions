# Problem: E.pas

```pascal
program _E;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 300 * 1000;
var
	notc, tci, n, i: int32;
	a: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(f[i]); readln;

	end;
end.

```
