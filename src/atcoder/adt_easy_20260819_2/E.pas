program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #binary #search
uses
	math;
const
	NN = 200 * 1000;
	INF = 1000 * 1000 * 1000 + 1;
var
	n, i, x, l, r: int32;
	m, s: int64;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	for i := 1 to n do read(a[i]);
	readln;

	l := 0;
	r := INF + 1;

	while r-l > 1 do begin
		x := (r+l) div 2;
		s := 0;

		for i := 1 to n do inc(s, min(x, a[i]));

		if s <= m then
			l := x
		else
			r := x;
	end;

	if l = INF then
		writeln('infinite')
	else
		writeln(l);
end.
