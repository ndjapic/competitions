# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, i, j: int32;
	s: int64;
	l: array [1 .. nn] of int64;
	c: array [1 .. nn] of char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	for i := 1 to n do begin
		readln(c[i], l[i]);
		s := min(nn+1, s + l[i]);
	end;

	if s > nn then
		writeln('Too Long')
	else begin
		for i := 1 to n do
			for j := 1 to l[i] do
				write(c[i]);
		writeln;
	end;
end.

```
