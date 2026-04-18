program D_183183;
uses
	math;
const
	nn = 200 * 1000;
var
	n, m, i, x: int32;
	e: int8;
	ans: int64;
	a, cp: array [1 .. nn] of int64;
	b: array [1 .. nn, 0 .. 10] of int64;

procedure sort(lend, rend: int32; e: int8);
var
	i, l, r, m: int32;

begin
	if rend - lend > 1 then begin
		m := (lend + rend) div 2;
		sort(lend, m, e);
		sort(m, rend, e);

		l := lend;
		r := m;
		for i := lend to rend - 1 do
			if (r = rend) or (l < m) and (b[l, e] <= b[r, e]) then begin
				cp[i] := b[l, e];
				inc(l);
			end else begin
				cp[i] := b[r, e];
				inc(r);
			end;

		for i := lend to rend - 1 do b[i, e] := cp[i];
	end;
end;

function bisect(x: int32; e: int8): int32;
var
	l, r, m: int32;
begin
	l := 0;
	r := n+1;
	while r-l > 1 do begin
		m := (l+r) div 2;
		if b[m, e] < x then
			l := m
		else
			r := m;
	end;
	bisect := l;
end;

begin
	readln(n, m);

	for i := 1 to n do begin
		read(a[i]);
		b[i, 0] := (a[i] +m-1) mod m +1;
		for e := 1 to 10 do b[i, e] := (b[i, e-1] * 10) mod m;
	end;
	readln;

	for e := 1 to 10 do sort(1, n+1, e);

	ans := 0;
	for i := 1 to n do begin

		e := 0;
		while a[i] > 0 do begin
			a[i] := a[i] div 10;
			inc(e);
		end;

		x := m - b[i, 0];
		inc(ans, bisect(x+1, e) - bisect(x, e));

	end;

	writeln(ans);
end.
