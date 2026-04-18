program B_Magic_Square;
const
	nn = 99;
var
	n, i, k, r, c, r1, c1: int32;
	a: array [0 .. nn, 0 .. nn] of int32;

begin
	readln(n);

	for r := 0 to n-1 do
		for c := 0 to n-1 do a[r, c] := -1;

	r := 0;
	c := n div 2;
	k := 1;
	a[r, c] := k;

	for i := 2 to n*n do begin
		inc(k);
		r1 := (r-1+n) mod n;
		c1 := (c+1) mod n;

		if a[r1, c1] = -1 then
			a[r1, c1] := k
		else begin
			r1 := (r+1) mod n;
			c1 := c;
			a[r1, c1] := k
		end;

		r := r1;
		c := c1;
	end;

	for r := 0 to n-1 do begin
		for c := 0 to n-2 do write(a[r, c], ' ');
		writeln(a[r, n-1]);
	end;
end.
