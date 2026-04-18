# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	c1, c2: char;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, c1, c1, c2, c2);
	readln(s);
	n := length(s);

	for i := 1 to n do
		if s[i] <> c1 then s[i] := c2;

	writeln(s);
end.

```
