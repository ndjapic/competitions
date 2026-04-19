# Problem: A_Equal_Occurrences.pas

```pascal
program A_Equal_Occurrences;
uses
	math;
const
	nn = 100;
var
	notc, tci: int16;
	n, i, m, j, x, mx, ans: int8;
	a, c, cc: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		read(a[1]);
		c[1] := 1;
		mx := 1;

		m := 1;
		for i := 2 to n do begin
			read(a[i]);
			if a[i-1] < a[i] then begin
				inc(m);
				c[m] := 0;
			end;
			inc(c[m]);
			mx := max(mx, c[m]);
		end;
		readln;

		for x := 0 to mx do cc[x] := 0;
		for j := 1 to m do inc(cc[c[j]]);

		ans := 0;
		for x := mx downto 1 do begin
			ans := max(ans, x * cc[x]);
			inc(cc[x-1], cc[x]);
		end;
		writeln(ans);

	end;
end.

```
