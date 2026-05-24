program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	writeln((1 shl n) - 2*n);
end.
