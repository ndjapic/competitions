program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	if (n > 5) or (1 shl n > n*n) then
		writeln('Yes')
	else
		writeln('No');
end.
