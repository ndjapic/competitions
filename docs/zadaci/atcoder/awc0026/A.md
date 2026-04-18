# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, i, a: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do begin
		read(a);
		if i mod k = 0 then write(a, ' ');
	end;
	readln;
	writeln;
end.

```
