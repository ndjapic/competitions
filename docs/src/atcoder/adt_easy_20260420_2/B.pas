program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := (1 shl n) - 2*n;

	writeln(ans);
end.
