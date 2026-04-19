# Problem: B_Niko_s_Tactical_Cards.pas

```pascal
program B_Niko_s_Tactical_Cards;
uses
	math;
const
	nn = 100 * 1000;
var
	notc, tci, n, i: int32;
	a, b: array [1 .. nn] of int32;
	l, r: array [0 .. nn] of int64;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;
		for i := 1 to n do read(b[i]); readln;

		l[0] := 0;
		r[0] := 0;

		for i := 1 to n do begin
			l[i] := min(l[i-1] - a[i], b[i] - r[i-1]);
			r[i] := max(r[i-1] - a[i], b[i] - l[i-1]);
		end;

		writeln(r[n]);

	end;
end.

```
