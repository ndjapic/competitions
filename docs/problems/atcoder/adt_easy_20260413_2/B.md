# Problem: B.pas

```pascal
program _B; (* WA *)
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x: real;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);
	writeln(round(x));
end.

```
