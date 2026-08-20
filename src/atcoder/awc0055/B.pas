program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, p, q, t, a, d: int32;
	ans: int32;
	x, c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, p, q);

	t := 1;
	a := 1;
	for i := 1 to n do begin
		readln(x[i], c[i]);
		d := abs(x[i] - p) - abs(x[t] - p);
		if (d < 0) or (d = 0) and (x[i] < x[t]) then t := i;
		d := abs(x[i] - q) - abs(x[a] - q);
		if (d < 0) or (d = 0) and (x[i] < x[a]) then a := i;
	end;

	ans := c[t] + 2;
	if t <> a then inc(ans, c[a]);
	writeln(ans);
end.
