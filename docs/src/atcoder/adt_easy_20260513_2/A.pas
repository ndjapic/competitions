program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	x, y: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y);

	writeln(max(0, y-x+9) div 10);
end.
