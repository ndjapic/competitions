# Задатак: Problem_A2_Snake_Scales_Chapter_2.pas

```pascal
program Problem_A2_Snake_Scales_Chapter_2;
const
	nn = 500 * 1000;
	inf = 1000 * 1000 * 1000;
var
	ntc, tci, n, i, l, r, m: int32;
	a: array [0 .. nn] of int32;
	dsu, size: array [0 .. nn] of int32;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union2(u, v: int32);
begin
	dsu[v] := u;
	inc(size[u], size[v]);
end;

procedure union1(u, v: int32);
begin
	u := find(u);
	v := find(v);
	if u = v then
	else if size[u] > size[v] then
		union2(u, v)
	else
		union2(v, u);
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;
		a[0] := 0;

		l := 0;
		r := inf;
		while r-l > 1 do begin
			m := (l+r) div 2;

			for i := 0 to n do begin
				dsu[i] := i;
				size[i] := 1;
			end;

			for i := 1 to n do begin
				if a[i] <= m then union1(i, 0);
				if abs(a[i] - a[i-1]) <= m then union1(i, i-1);
			end;

			i := 1;
			while (i <= n) and (find(i) = find(0)) do inc(i);

			if i > n then
				r := m
			else
				l := m;
		end;

		writeln('Case #', tci, ': ', r);

	end;
end.

```
