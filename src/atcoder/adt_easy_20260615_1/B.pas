program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	strutils;
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	n := length(s);
	i := n;

	while s[i] <> '.' do dec(i);

	writeln(rightstr(s, n-i));
end.
