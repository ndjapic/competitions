program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	l, r: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(l, r);

	if l = r then
		writeln('Invalid')
	else if l = 1 then
		writeln('Yes')
	else
		writeln('No');
end.
