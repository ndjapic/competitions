# Задатак: B_Split.pas

```pascal
program B_Split;
uses
	math;
const
	nn2 = 400 * 1000;
var
	notc, tci, n, i, x, c1, c2, c3, c4, ans: int32;
	a, c: array [1 .. nn2] of int32;

begin
	for x := 1 to nn2 do c[x] := 0;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		c1 := 0;
		c2 := 0;
		c3 := 0;
		c4 := 0;

		for i := 1 to 2*n do begin

			read(x);
			a[i] := x;
			inc(c[x]);

			if c[x] = 1 then
				inc(c1)
			else if c[x] = 2 then begin
				dec(c1);
				inc(c2);
			end else if c[x] mod 4 = 1 then begin
				dec(c4);
				inc(c3);
			end else if c[x] mod 4 = 3 then begin
				dec(c2);
				inc(c3);
			end else if c[x] mod 4 = 0 then begin
				dec(c3);
				inc(c4);
			end else begin
				dec(c3);
				inc(c2);
			end;

		end;
		readln;

		ans := c1 + c3 + 2 * c2 + 2 * c4;
		if odd(c4) and (c1 + c3 = 0) then dec(ans, 2);

		for i := 1 to 2*n do begin
			x := a[i];
			c[x] := 0;
		end;

		writeln(ans);

	end;
end.

```
