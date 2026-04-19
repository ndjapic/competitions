# Problem: D_Antiamuny_Wants_to_Learn_Swap.pas

```pascal
program D_Antiamuny_Wants_to_Learn_Swap;
const
	nn = 500 * 1000;
var
	ntc, tci, n, i, q, l, r: int32;
	a: array [1 .. nn] of int32;
	prev: array [0 .. nn] of int32;
	perfect: array [1 .. nn] of boolean;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, q);

		for i := 1 to n do read(a[i]); readln;

		for i := 2 to n-1 do
			perfect[i] := not ((a[i-1] > a[i]) and (a[i] > a[i+1]));
		perfect[1] := true;
		perfect[n] := true;

		prev[0] := 0;
		for i := 1 to n do
			if perfect[i] then
				prev[i] := prev[i-1]
			else
				prev[i] := i;

		for i := 1 to q do begin
			readln(l, r);
			if prev[r-1] <= l then
				writeln('YES')
			else
				writeln('NO');
		end;

	end;
end.

```
