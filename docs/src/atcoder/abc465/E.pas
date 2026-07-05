program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
var
	n, i, j: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
end.
