# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	hh = 500;
var
	h, w, a, b, c, d, i, j: int32;
	s: array [1 .. hh] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	a := h;
	b := 1;
	c := w;
	d := 1;

	for i := 1 to h do begin
		readln(s[i]);

		for j := 1 to w do
			if s[i][j] = '#' then begin
				a := min(a, i);
				b := max(b, i);
				c := min(c, j);
				d := max(d, j);
			end;
	end;

	for i := a to b do
		for j := c to d do
			if s[i][j] = '.' then writeln(i, ' ', j);
end.

```
