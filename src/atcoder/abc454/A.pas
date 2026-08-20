program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	l, r: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(l, r);
	writeln(r-l+1);
end.
