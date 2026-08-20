program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	writeln(n+1-k);
end.
