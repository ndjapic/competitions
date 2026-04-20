program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, l, r, x, c: int8;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	l := 0;
	r := n+1;
	while r-l > 1 do begin
		x := (l+r) div 2;

		c := 0;
		for i := 1 to n do
			if a[i] >= x then inc(c);

		if c >= x then
			l := x
		else
			r := x;
	end;

	writeln(l);
end.
