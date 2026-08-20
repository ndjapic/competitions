program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, l, r, x: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	l := 1;
	x := -1;
	for r := 1 to n do
		if s[r] = '-' then
			l := r+1
		else if (l > 1) and (s[l-1] = '-') then
			x := max(x, r-l+1)
		else if (r < n) and (s[r+1] = '-') then
			x := max(x, r-l+1);

	writeln(x);
end.
