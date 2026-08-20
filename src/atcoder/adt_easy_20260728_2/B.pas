program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, a, b: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);
	readln(s);

	writeln(copy(s, a+1, n-a-b));
end.
