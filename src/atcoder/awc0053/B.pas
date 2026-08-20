program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	inf = 1 shl 30;
var
	n, i, l, r, m, lc, rc: int32;
	ans: int64;
	x: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(x[i]);
	readln;

	l := -inf;
	r := inf;
	while r-l > 1 do begin
		m := (l+r) div 2;

		lc := 0;
		rc := 0;
		for i := 1 to n do
			if x[i] < m then
				inc(lc)
			else
				inc(rc);

		if lc <= rc then
			l := m
		else
			r := m;
	end;

	ans := 0;
	for i := 1 to n do
		inc(ans, abs(x[i] - l));

	writeln(ans);
end.
