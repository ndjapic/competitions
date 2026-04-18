# Задатак: D_Price_Tags.pas

```pascal
program D_Price_Tags;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, x, ci, l, r: int32;
	y: int64;
	total: array [1 .. nn] of int64;
	c, have, need: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, y);

		total[1] := 0;
		total[nn] := n;
		for i := 1 to n do begin
			read(c[i]);
			inc(total[1], c[i]);
			if c[i] = 1 then dec(total[nn], y);
		end;
		readln;

		l := 1;
		r := nn;
		while r-l > 1 do begin

			x := (l+r) div 2;

			for ci := 1 to nn do begin
				have[ci] := 0;
				need[ci] := 0;
			end;

			for i := 1 to n do begin
				ci := c[i];
				inc(have[ci]);
				ci := (ci + x - 1) div x;
				inc(need[ci]);
			end;

			total[x] := 0;
			for ci := 1 to nn do begin
				inc(total[x], int64(ci) * need[ci]);
				inc(total[x], int64(y) * max(need[ci] - have[ci], 0));
			end;

			if total[r] >= total[l] then
				l := x
			else
				r := x;

		end;

		writeln(r);

	end;
end.

```
