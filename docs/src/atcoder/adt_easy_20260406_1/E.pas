program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 500 * 1000;
var
	n, m, mn, l, r: int32;
	k: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k);
	readln(s);
	readln(t);
	n := length(s);
	m := length(t);
	mn := min(n, m);

	l := 0;
	r := 0;

	while (l < mn) and (s[1+l] = t[1+l]) do inc(l);
	while (r < mn) and (s[n-r] = t[m-r]) do inc(r);

	if (l+r >= mn) or (n = m) and (l+r = mn-1) then
		writeln('Yes')
	else
		writeln('No');
end.
