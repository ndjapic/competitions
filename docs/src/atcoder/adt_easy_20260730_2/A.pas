program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	r: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r);

	writeln(100 - r mod 100);
end.
