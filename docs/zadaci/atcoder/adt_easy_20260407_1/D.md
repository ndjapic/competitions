# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, q, i, c, x: int8;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for x := 1 to n do a[x] := 0;

	for i := 1 to q do begin
		readln(c, x);
		case c of
			1: inc(a[x]);
			2: inc(a[x], 2);
			3: if a[x] >= 2 then
				writeln('Yes')
			else
				writeln('No');
		end;
	end;
end.

```
