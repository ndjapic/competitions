program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c);

	if (a <= b) and (b <= c) then
		writeln('Yes')
	else if (a >= b) and (b >= c) then
		writeln('Yes')
	else
		writeln('No');
end.
