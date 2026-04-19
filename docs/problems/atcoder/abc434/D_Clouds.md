# Problem: D_Clouds.pas

```pascal
program D_Clouds;
uses
	math;
const
	rows = 2000;
	nn = 200 * 1000;
var
	n, i, row, col, mn, mx, ans: int32;
	pre, suf: array [1 .. nn, 1 .. rows] of record
		l, r: int32;
	end;
	u, d, l, r: array [1 .. nn] of int32;

begin
	readln(n);
	for i := 1 to n do readln(u[i], d[i], l[i], r[i]);

	for row := 1 to rows do begin
		pre[1, row].l := rows + 1;
		pre[1, row].r := 0;
	end;

	for i := 1 to n-1 do
		for row := u[i] to d[i] do begin
			pre[i+1, row].l := min(pre[i, row].l, l[i]);
			pre[i+1, row].r := max(pre[i, row].r, r[i]);
		end;

	for row := 1 to rows do begin
		suf[n, row].l := rows + 1;
		suf[n, row].r := 0;
	end;

	for i := n downto 2 do
		for row := u[i] to d[i] do begin
			suf[i-1, row].l := min(suf[i, row].l, l[i]);
			suf[i-1, row].r := max(suf[i, row].r, r[i]);
		end;

	for i := 1 to n do begin
		ans := sqr(rows);
		for row := 1 to rows do begin
			mn := min(pre[i, row].l, suf[i, row].l);
			mx := max(pre[i, row].r, suf[i, row].r);
			dec(ans, max(0, mx - mn + 1));
		end;
	end;
end.

```
