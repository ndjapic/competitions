program C_Coloring_Game;
uses
	math;
const
	nn = 5000;
var
	ntc, tci, n, i, j, k: int16;
	ans: int64;
	a, l, r: array [1 .. nn] of int32;

function bisect(x: int32): int16;
var
	l, r, m: int16;
begin
	l := 1;
	r := n+1;
	while l < r do begin
		m := (l+r) div 2;
		if x < a[m] then
			r := m
		else
			l := m+1;
	end;
	bisect := l;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;

		ans := 0;
		for i := 1 to n-2 do begin

			k := i+2;
			for j := i+1 to n-1 do begin
				while (k <= n) and (a[k] < a[i] + a[j]) do inc(k);
				r[j] := k-1;
			end;

			k := n-1;
			for j := n-1 downto i+1 do begin
				while (k > j) and (a[i] + a[j] + a[k] > a[n]) do dec(k);
				l[j] := k+1;
			end;

			for j := i+1 to n-1 do
				inc(ans, max(r[j] - l[j] + 1, 0));

		end;

		writeln(ans);

	end;
end.
