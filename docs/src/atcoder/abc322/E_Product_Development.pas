program E_Product_Development;
const
	maxn = 100;
var
	n, k, p, i, j: int8;
	a: array [1 .. maxn, 1 .. 5] of int8;
	c: array [1 .. maxn] of int32;
	dp: array [0 .. maxn, 1 .. 5, 0 .. 5] of int64;

begin
	readln(n, k, p);
	for i := 1 to n do begin
		read(c[i]);
		for j := 1 to k do read(a[i, j]);
		readln;
	end;
end.
