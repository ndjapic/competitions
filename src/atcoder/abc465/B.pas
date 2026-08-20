program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	x, y, l, r, a, b, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y, l, r, a, b);

	l := max(l, a);
	r := min(r, b);

	ans := (b-a) * y + (x-y) * max(0, r-l);
	writeln(ans);
end.
