program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000;
var
	n, i: int32;
	m, s, a, b, c: int64;
	r, x: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	s := 0;
	a := -1;
	b := 0;

	for i := 1 to n do begin
		read(r[i]);
		s := min(s + r[i], m);
		b := max(b, r[i]);
	end;
	readln;

	if s < m then
		writeln(-1)
	else begin

		while b-a > 1 do begin
			c := (b+a) div 2;

			s := 0;
			for i := 1 to n do begin
				x[i] := max(0, r[i] - c);
				s := min(s + x[i], m+1);
			end;

			if s <= m then
				b := c
			else
				a := c;
		end;

		writeln(b);

	end;
end.
