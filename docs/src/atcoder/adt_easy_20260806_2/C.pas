program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bisect
const
	NN = 100;
var
	n, i, t, p, c, l, r, m: int8;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t, p);

	for i := 1 to n do read(a[i]);
	readln;

	l := -1;
	r := NN;
	while r-l > 1 do begin
		m := (r+l) div 2;

		c := 0;
		for i := 1 to n do
			if a[i] >= t-m then inc(c);

		if c >= p then
			r := m
		else
			l := m;
	end;

	writeln(r);
end.
