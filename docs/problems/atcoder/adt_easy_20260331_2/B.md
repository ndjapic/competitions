# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	r, c, i, j: int8;
	a: array [1 .. 2, 1 .. 2] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, c);

	for i := 1 to 2 do begin
		for j := 1 to 2 do read(a[i, j]);
		readln;
	end;

	writeln(a[r, c]);
end.

```
