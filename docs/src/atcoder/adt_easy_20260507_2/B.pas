program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, x, y, a, b: int16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y, n);

	a := n * x;
	b := n div 3 * y + n mod 3 * x;

	writeln(min(a, b));
end.
