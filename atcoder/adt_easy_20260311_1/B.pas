program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Classes;
var
	a, b: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);
	writeln(a or b);
end.
