# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	m: int64;
	a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		read(a);
		m := (m * a) div 100;
	end;
	readln;

	writeln(m);
end.

```
