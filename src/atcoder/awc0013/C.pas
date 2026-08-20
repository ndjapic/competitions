program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, k, i, c: int32;
	s: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(s[i]); readln;

	c := 0;
	for i := 1 to n do
		if s[i] < s[k] then inc(c);
	writeln(c);
end.
