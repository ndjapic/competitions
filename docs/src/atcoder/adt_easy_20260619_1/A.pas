program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c, x: int32;
	ans: real;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c, x);

	if x <= a then
		ans := 1.0
	else if x > b then
		ans := 0.0
	else
		ans := c / (b-a);

	writeln(ans:0:6);
end.
