program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	inf = int64(1) shl 60;
var
	n, m, i, j, l, r: int32;
	productivity, mn, mx: int64;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(a[i]);
	readln;

	mn := inf;
	mx := 0;

	for j := 1 to m do begin
		readln(l, r);
		productivity := 0;
		for i := l to r do inc(productivity, a[i]);
		mn := min(mn, productivity);
		mx := max(mx, productivity);
	end;

	writeln(mx - mn);
end.
