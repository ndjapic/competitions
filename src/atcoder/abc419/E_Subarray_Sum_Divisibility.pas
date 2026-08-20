program E_Subarray_Sum_Divisibility;
uses
	math;
const
	nn = 500;
var
	n, m, l, i, j, c: int16;
	s, ans: int32;
	a, mn, mx, d: array [0 .. nn] of int16;

begin
	readln(n, m, l);

	for i := 0 to n-1 do read(a[i]); readln;

	s := 0;
	for c := 0 to l-1 do begin
		d[c] := m-1;
		inc(s, a[c]);
	end;

	ans := m - s mod m;
	if ans = m then ans := 0;
	inc(a[0], ans);
	if a[0] >= m then dec(a[0], m);

	for j := 0 to m-1 do begin

		for c := 0 to l-1 do begin
			mn[c] := m-1;
			mx[c] := 0;
		end;

		for i := 0 to n-1 do begin
			c := i mod l;
			mn[c] := min(mn[c], a[i]);
			mx[c] := max(mx[c], a[i]);
			inc(a[i]);
			if a[i] = m then a[i] := 0;
		end;

		for c := 0 to l-1 do d[c] := min(d[c], mx[c] - mn[c]);

	end;

	for c := 0 to l-1 do inc(ans, (d[c] + 1) div 2);
	writeln(ans);
end.
