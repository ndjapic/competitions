program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	l1, r1, l2, r2, l, r: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(l1, r1, l2, r2);

	l := max(l1, l2);
	r := min(r1, r2);

	writeln(max(r-l, 0));
end.
