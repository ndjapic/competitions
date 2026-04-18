program D_Takahashi_s_Expectation;
uses
	math;
const
	nn = 10 * 1000 + 1;
	qq = 500 * 1000;
	xl = 1 - 500;
	xr = 500 + 500;
var
	n, i, q, j: int32;
	p, a, b: array [1 .. nn] of int32;
	x: array [1 .. qq] of int32;
	{dp: array [0 .. nn, xl .. xr] of int32;}

begin
	readln(n);
	for i := 1 to n do readln(p[i], a[i], b[i]);

	readln(q);
	for j := 1 to q do begin
		read(x[j]);

		for i := 1 to n do
			if p[i] >= x[j] then
				inc(x[j], a[i])
			else if x[j] < b[i] then
				x[j] := 0
			else
				dec(x[j], b[i]);

		writeln(' ', x[j]);
	end;
	readln;
end.
