# Problem: D_Robot_Customize.pas

```pascal
program D_Robot_Customize;
uses
	math;
const
	nn = 500;
var
	n, i, j, rank, s, c: int32;
	w, h, b, p, cp: array [1 .. nn] of int32;
	toHead: array [1 .. nn] of boolean;

procedure MergeSort(lend, rend: int32);
var
	i, l, r, m: int32;
begin
	if rend - lend > 1 then begin
		m := (lend + rend) div 2;
		MergeSort(lend, m);
		MergeSort(m, rend);

		l := lend;
		r := m;
		for i := lend to rend - 1 do
			if (r = rend) or (l < m) and (
				int64(h[l] - b[l]) * w[r] >=
				int64(h[r] - b[r]) * w[l]
			) then begin
				cp[i] := p[l];
				inc(l);
			end else begin
				cp[i] := p[r];
				inc(r);
			end;

		for i := lend to rend - 1 do p[i] := cp[i];
	end;
end;

begin
	readln(n);

	s := 0;
	for i := 1 to n do begin
		readln(w[i], h[i], b[i]);
		inc(s, w[i]);
		p[i] := i;
	end;
	s := s div 2;
	MergeSort(1, n+1);

	dw[0, 0] := 0;
	dh[0, 0] := 0;

	c := 0;
	for r := 1 to n do begin

		if c + w[r] <= s then begin
			inc(c, w[r]);
			toHead[r] := true;
		end else begin
			toHead[r] := false;
			for l := 1 to r-1 do
				if toHead[l] then
		end;

	end;
end.

```
