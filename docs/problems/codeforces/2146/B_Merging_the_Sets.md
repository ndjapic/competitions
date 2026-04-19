# Problem: B_Merging_the_Sets.pas

```pascal
program B_Merging_the_Sets;
const
	nn = 50 * 1000;
	mm = 100 * 1000;
var
	notc, tci, n, m, i, j, x, c1: int32;
	s: array [1 .. nn] of array of int32;
	l: array [1 .. nn] of int32;
	c: array [1 .. mm] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m);

		for x := 1 to m do c[x] := 0;

		for i := 1 to n do begin
			read(l[i]);
			setlength(s[i], l[i]);
			for j := 0 to l[i]-1 do begin
				read(x);
				s[i][j] := x;
				inc(c[x]);
			end;
			readln;
		end;

		x := 1;
		while (x <= m) and (c[x] > 0) do inc(x);

		c1 := 0;
		i := 1;
		while (i <= n) and (c1 < 2) do begin
			j := 0;
			while (j < l[i]) and (c[s[i][j]] = 1) do inc(j);
			if j < l[i] then inc(c1);
			inc(i);
		end;

		if (x > m) and (c1 >= 2) then
			writeln('YES')
		else
			writeln('NO');

	end;
end.

```
