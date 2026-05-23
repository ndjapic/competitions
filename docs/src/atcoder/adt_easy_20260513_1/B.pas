program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c, x: int32;
	answer: extended;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c, x);

	if x <= a then
		answer := 1
	else if x <= b then
		answer := extended(1.0) * c / (b-a)
	else
		answer := 0;

	writeln(answer:0:12);
end.
