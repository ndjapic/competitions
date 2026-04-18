# Задатак: B_Pile_Shuffling.pas

```pascal
program B_Pile_Shuffling;
uses
	math;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i: int32;
	ans: int64;
	a, b, c, d: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		ans := 0;

		for i := 1 to n do begin
			readln(a[i], b[i], c[i], d[i]);
			if b[i] > d[i] then
				inc(ans, a[i] + b[i] - d[i])
			else if a[i] > c[i] then
				inc(ans, a[i] - c[i]);
		end;

		writeln(ans);

	end;
end.

```
