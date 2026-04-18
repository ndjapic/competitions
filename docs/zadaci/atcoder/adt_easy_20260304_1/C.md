# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, l, r: int8;
	lock: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(lock[i]); readln;

	l := 0;
	r := n;
	while (r-l > 1) and (lock[l+1] = 0) do inc(l);
	while (r-l > 1) and (lock[r] = 0) do dec(r);

	writeln(r-l-1);
end.

```
