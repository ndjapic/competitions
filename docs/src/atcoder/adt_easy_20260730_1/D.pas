program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	if x < 0 then dec(x, 9);

	writeln(x div 10);
end.
