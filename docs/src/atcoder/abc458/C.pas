program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i: int32;
	ans: int64;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	ans := 0;
	for i := 1 to n do
		if s[i] = 'C' then
			inc(ans, min(i, n-i+1));

	writeln(ans);
end.
