program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, y: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y);
	writeln(x shl y);
end.
