program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	y, d: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(y);

	d := (3002 - y) mod 4;

	writeln(y + d);
end.
