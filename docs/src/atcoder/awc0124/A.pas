program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, l, r, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	l := 0;
	ans := 0;

	for r := 1 to n do
		if s[r] = 'L' then
			l := r
		else
			ans := max(ans, r-l);

	writeln(ans);
end.
