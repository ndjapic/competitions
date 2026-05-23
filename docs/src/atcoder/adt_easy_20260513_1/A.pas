program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	l, r: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(l, r);
	s := 'atcoder';

	writeln(copy(s, l, r-l+1));
end.
