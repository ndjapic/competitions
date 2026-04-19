# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, j, i0, u, v, w: int32;
	p: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(p[i]); readln;

	for j := 1 to m do begin
		readln(u, v, w);
		dec(p[u], w);
		inc(p[v], w);
	end;

	i0 := 1;
	for i := 2 to n do
		if p[i] > p[i0] then i0 := i;

	writeln(i0);
end.

```
