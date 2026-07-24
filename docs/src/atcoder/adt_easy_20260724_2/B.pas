program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(n);

	m := length(s);

	writeln(copy(s, n+1, m-2*n));
end.
