# Задатак: B_No_Divisible_Range.pas

```pascal
program B_No_Divisible_Range;
const
	nn = 50;
var
	n, i, l, r, ans: int32;
	a, s: array [0 .. nn] of int32;

begin
	readln(n);
	s[0] := 0;
	ans := 0;

	for r := 1 to n do begin
		read(a[r]);
		s[r] := s[r-1] + a[r];
		for l := 1 to r do begin
			i := l;
			while (i <= r) and ((s[r] - s[l-1]) mod a[i] > 0) do
				inc(i);
			if i > r then inc(ans);
		end;
	end;

	readln;
	writeln(ans);
end.

```
