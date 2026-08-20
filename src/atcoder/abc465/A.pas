program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	if 3 * a > 2 * b then
		writeln('Yes')
	else
		writeln('No');
end.
