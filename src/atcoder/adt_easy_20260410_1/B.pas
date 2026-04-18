program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	s, a, b, x, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s, a, b, x);

	ans := x div (a+b) * s * a + s * min(a, x mod (a+b));

	writeln(ans);
end.
