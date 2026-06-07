program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	sysutils;
var
	n: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	writeln(rightstr(n, 2));
end.
