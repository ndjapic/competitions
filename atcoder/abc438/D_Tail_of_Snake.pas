program D_Tail_of_Snake;
uses
	math;
const
	nn = 300 * 1000;
var
	n, i, x, y: int32;
	ans: int64;
	a, b, c: array [0 .. nn] of int64;

begin
	readln(n);

	a[0] := 0;
	for i := 1 to n do begin
		read(a[i]);
		inc(a[i], a[i-1]);
	end;
	readln;

	b[0] := 0;
	for i := 1 to n do begin
		read(b[i]);
		inc(b[i], b[i-1]);
	end;
	readln;

	c[0] := 0;
	for i := 1 to n do begin
		read(c[i]);
		inc(c[i], c[i-1]);
	end;
	readln;

	ans := 0;
	for x := 1 to n-2 do
		for y := x+1 to n-1 do
			ans := max(ans, a[x] + b[y] - b[x] + c[n] - c[y]);

	writeln(ans);
end.
