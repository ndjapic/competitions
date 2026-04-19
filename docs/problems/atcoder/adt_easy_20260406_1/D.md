# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, q, i, p, a, b: int8;
	inv: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do begin
		read(p);
		inv[p] := i;
	end;
	readln;

	readln(q);
	for i := 1 to q do begin
		read(a, b);
		if inv[a] < inv[b] then
			writeln(a)
		else
			writeln(b);
	end;
end.

```
