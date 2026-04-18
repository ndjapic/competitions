# Задатак: C_Triple_Removal.pas

```pascal
program C_Triple_Removal;
uses
	math;
const
	nn = 250 * 1000;
var
	notc, tci, n, q, i, l, r, ans: int32;
	a: array [0 .. nn] of int8;
	x: array [0 .. nn] of int32;
	c: array [0 .. 1, 0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, q);

		c[0, 0] := 0;
		c[1, 0] := 0;
		a[0] := 0;
		x[0] := 0;

		for i := 1 to n do begin
			c[0, i] := c[0, i-1];
			c[1, i] := c[1, i-1];
			read(a[i]);
			inc(c[a[i], i]);
			x[i] := x[i-1] + 1 - (a[i-1] xor a[i]);
		end;
		readln;

		for i := 1 to q do begin

			readln(l, r);

			if (c[0, r] - c[0, l-1]) mod 3 > 0 then
				ans := -1
			else if (c[1, r] - c[1, l-1]) mod 3 > 0 then
				ans := -1
			else begin
				ans := (r-l+1) div 3;
				if x[r] - x[l] = 0 then inc(ans);
			end;

			writeln(ans);

		end;

	end;
end.

```
