program D_Division_Versus_Addition;
uses
	math;
const
	nn = 250 * 1000;
var
	notc, tci, n, q, i, x, l, r, ans: int32;
	e: int8;
	a: array [1 .. nn] of int32;
	c, s: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, q);

		s[0] := 0;
		c[0] := 0;

		for i := 1 to n do begin
			read(x);
			a[i] := x;

			e := 0;
			s[i] := s[i-1];
			while x > 1 do begin
				inc(s[i]);
				x := x div 2;
				inc(e);
			end;

			c[i] := c[i-1];
			if a[i] > x shl e then inc(c[i]);
		end;
		readln;

		for i := 1 to q do begin
			readln(l, r);
			ans := s[r] - s[l-1];
			inc(ans, max(c[r] - c[l-1] - 1, 0));
			writeln(ans);
		end;

	end;
end.
