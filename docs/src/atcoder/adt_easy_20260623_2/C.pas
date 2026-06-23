program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	a, b, c, d, e, f: int32;
	g, h, i, j, k, l: int32;
	x1, y1, z1, x2, y2, z2: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c, d, e, f);
	readln(g, h, i, j, k, l);

	x1 := max(a, g);
	y1 := max(b, h);
	z1 := max(c, i);

	x2 := min(d, j);
	y2 := min(e, k);
	z2 := min(f, l);

	if (x1 < x2) and (y1 < y2) and (z1 < z2) then
		writeln('Yes')
	else
		writeln('No');
end.
