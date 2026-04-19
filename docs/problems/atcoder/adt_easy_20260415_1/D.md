# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	a, b: int32;
	r, x, y: real;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	r := sqrt(sqr(a) + sqr(b));
	x := a / r;
	y := b / r;

	writeln(x:0:6, ' ', y:0:6);
end.

```
