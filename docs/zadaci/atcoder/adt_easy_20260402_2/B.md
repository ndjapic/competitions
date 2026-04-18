# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ai: int8;
	s: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	for i := 1 to n-1 do begin
		read(ai);
		inc(s, ai);
	end;
	readln;

	writeln(-s);
end.

```
