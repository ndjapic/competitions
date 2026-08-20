program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	s, a, b, x, d: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s, a, b, x);

	d := x div (a+b) * b + max(0, x mod (a+b) - a);

	writeln(s * (x - d));
end.
