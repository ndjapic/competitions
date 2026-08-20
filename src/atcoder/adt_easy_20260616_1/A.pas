program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, t: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	t := (n+1) * n div 2;
	writeln(t);
end.
