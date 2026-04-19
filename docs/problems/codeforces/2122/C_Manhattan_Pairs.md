# Problem: C_Manhattan_Pairs.pas

```pascal
program C_Manhattan_Pairs;
const
	nn = 200 * 1000;
	xx = 1000 * 1000;
var
	ntc, tci, n, h, i, i1, j, k, l, r, m, c: int32;
	x, y, p, cp: array [1 .. nn] of int32;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (x[p[il]] <= x[p[ir]]) then begin
                cp[i] := p[il];
                inc(il);
            end else begin
                cp[i] := p[ir];
                inc(ir);
            end;

        for i := l to r-1 do p[i] := cp[i];

    end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		h := n div 2;

		for i := 1 to n do begin
			readln(x[i], y[i]);
			p[i] := i;
		end;
		msort(1, n+1);

		l := -xx;
		r := xx+1;
		while r-l > 1 do begin
			m := (l+r) div 2;

			c := 0;
			for i := 1 to n do
				if y[i] < m then inc(c);

			if c < h then
				l := m
			else
				r := m;
		end;

		c := 0;
		for i := 1 to n do
			if y[i] < l then inc(c);

		i1 := 0;
		while c < h do begin
			inc(i1);
			if y[i1] = l then inc(c);
		end;

		i := 1;
		j := 1;
		k := n;
		while i <= h do
			if (y[p[j]] > l) or (y[p[j]] = l) and (p[j] > i1) then
				inc(j)
			else if (y[p[k]] < l) or (y[p[k]] = l) and (p[k] <= i1) then
				dec(k)
			else begin
				writeln(p[j], ' ', p[k]);
				inc(i);
				inc(j);
				dec(k);
			end;

	end;
end.

```
