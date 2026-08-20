program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	writeln(n[2], n[3]);
end.
