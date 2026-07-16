program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	if (n > 20) or (1 shl n > sqr(n)) then
		writeln('Yes')
	else
		writeln('No');
end.
