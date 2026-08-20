program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 1000;
	inf = 1 shl 60;
var
	n, k, i, j: int32;
	dx, dy, mn, mx: int64;
	r: real;
	a, x, y: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for j := 1 to k do read(a[j]); readln;
	for i := 1 to n do readln(x[i], y[i]);

	mx := 0;
	for i := 1 to n do begin
		mn := inf;
		for j := 1 to k do begin
			dx := x[i] - x[a[j]];
			dy := y[i] - y[a[j]];
			mn := min(mn, sqr(dx) + sqr(dy));
		end;

		mx := max(mx, mn);
	end;

	r := sqrt(mx);
	writeln(r:0:5);
end.
