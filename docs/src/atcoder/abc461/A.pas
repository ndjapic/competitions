program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, d: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, d);

	if a > d then
		writeln('No')
	else
		writeln('Yes');
end.
