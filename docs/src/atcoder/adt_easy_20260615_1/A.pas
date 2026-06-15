program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	strutils;
var
	n: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := '3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679';

	writeln(leftstr(s, n+2));
end.
