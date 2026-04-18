# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, m, i, j, h: int8;
	s: array [1 .. nn] of string;
	c: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	m := 0;

	for i := 1 to n do begin
		readln(s[i]);
		c[i] := length(s[i]);
		m := max(m, c[i]);
	end;

	for i := 1 to n do begin
		h := (m - c[i]) div 2;
		for j := 1 to h do write('.');
		write(s[i]);
		for j := 1 to h do write('.');
		writeln;
	end;
end.

```
