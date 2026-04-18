# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, i0, l: int32;
	d: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l);

	i0 := -1;
	for i := 1 to n do begin
		read(d[i]);
		if (l >= d[i]) and ((i0 = -1) or (d[i] > d[i0])) then
			i0 := i;
	end;
	readln;

	writeln(i0);
end.

```
