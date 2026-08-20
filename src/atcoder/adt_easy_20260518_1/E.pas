program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000;
	AA = 1000 * 1000 * 1000;
	MM = NN * AA;
var
	n, i: int32;
	m, l, r, x, subsidy: int64;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(a[i]);
	readln;

	l := 0;
	r := MM + 2;
	while r-l > 1 do begin
		x := (l+r) div 2;

		subsidy := 0;
		for i := 1 to n do inc(subsidy, min(x, a[i]));

		if subsidy > m then
			r := x
		else
			l := x;
	end;

	if l > MM then
		writeln('infinite')
	else
		writeln(l);
end.
