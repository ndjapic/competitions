program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, y: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y);

	if (y <= x+2) and (y >= x-3) then
		writeln('Yes')
	else
		writeln('No');
end.
