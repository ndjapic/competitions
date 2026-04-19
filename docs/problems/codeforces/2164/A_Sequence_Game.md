# Problem: A_Sequence_Game.pas

```pascal
program A_Sequence_Game;
uses
	math;
const
	nn = 100;
	inf = int32(1) shl 30;
var
	notc, tci, n, i, x, mn, mx: int32;
	a: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		mn := inf;
		mx := -inf;

		for i := 1 to n do begin
			read(a[i]);
			mn := min(mn, a[i]);
			mx := max(mx, a[i]);
		end;
		readln;

		readln(x);

		if (mn <= x) and (x <= mx) then
			writeln('YES')
		else
			writeln('NO');

	end;
end.

```
