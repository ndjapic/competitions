program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	strutils;
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	writeln('0', leftstr(s, 3));
end.
