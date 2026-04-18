# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i: int8;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	n := 0;
	repeat
		inc(n);
		readln(a[n]);
	until a[n] = 0;

	for i := n downto 1 do writeln(a[i]);
end.

```
