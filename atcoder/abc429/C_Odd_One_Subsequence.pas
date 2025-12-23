program C_Odd_One_Subsequence;
const
	nn = 200 * 1000;
var
	n, i, x, y: int32;
	ans: int64;
	a, c: array [1 .. nn] of int32;

begin
	readln(n);

	for x := 1 to n do c[x] := 0;

	for i := 1 to n do begin
		read(a[i]);
		inc(c[a[i]]);
	end;
	readln;

	ans := 0;
	for x := 1 to n do begin
		y := c[x];
		inc(ans, int64(y-1) * y * (n-y));
	end;
	writeln(ans div 2);
end.
