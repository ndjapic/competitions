program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	d: real;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(d);
	writeln(sqr(d / 2) * pi :0:6);
end.
