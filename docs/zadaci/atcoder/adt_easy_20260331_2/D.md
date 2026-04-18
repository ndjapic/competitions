# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 50;
var
	h, w, i, j, i1, i2, j1, j2: int8;
	ans: boolean;
	a: array [1 .. hh, 1 .. hh] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do begin
		for j := 1 to w do read(a[i, j]);
		readln;
	end;

	ans := true;
	for i1 := 1 to h do
		for j1 := 1 to w do
			for i2 := i1+1 to h do
				for j2 := j1+1 to w do
					ans := ans and (a[i1, j1] + a[i2, j2] <= a[i2, j1] + a[i1, j2]);

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
