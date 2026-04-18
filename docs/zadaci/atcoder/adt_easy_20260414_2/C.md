# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	ww = 1000;
var
	h, w, i, j: int32;
	c: string;
	x: array [1 .. ww] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for j := 1 to w do x[j] := 0;

	for i := 1 to h do begin
		readln(c);
		for j := 1 to w do
			if c[j] = '#' then inc(x[j]);
	end;

	for j := 1 to w-1 do write(x[j], ' ');
	writeln(x[w]);
end.

```
