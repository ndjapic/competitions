program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	if m <= (n+1) div 2 then
		writeln('Yes')
	else
		writeln('No');
end.
