program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	a, b, c, d, e, f, g, h, i, j, k, l: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c, d, e, f);
	readln(g, h, i, j, k, l);

	a := max(a, g);
	b := max(b, h);
	c := max(c, i);

	d := min(d, j);
	e := min(e, k);
	f := min(f, l);

	if (a < d) and (b < e) and (c < f) then
		writeln('Yes')
	else
		writeln('No');
end.
