program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	l, r: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(l, r);
	s := 'atcoder';
	writeln(copy(s, l, r-l+1));
end.
