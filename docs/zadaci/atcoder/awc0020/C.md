# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, v, l, r: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(v);
	l := v;
	r := v;

	for i := 2 to n do begin
		read(v);
		l := min(l, v);
		r := max(r, v);
	end;
	readln;

	writeln(r-l);
end.

```
