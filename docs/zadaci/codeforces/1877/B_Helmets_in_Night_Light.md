# Задатак: B_Helmets_in_Night_Light.pas

```pascal
program B_Helmets_in_Night_Light;
uses
	math;
const
	maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, p, i, l, r, d: int32;
    ans: int64;
    res, merge: array [1 .. maxn] of record
		a, b: int32;
    end;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (res[il].b <= res[ir].b) then begin
                merge[i] := res[il];
                inc(il);
            end else begin
                merge[i] := res[ir];
                inc(ir);
            end;

        for i := l to r do res[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, p);
		for i := 1 to n do read(res[i].a); readln;
		for i := 1 to n do read(res[i].b); readln;
		msort(1, n);

		l := 1;
		r := 1;
		ans := p;

		while (r < n) and (res[l].b < p) do begin
			d := min(n-r, res[l].a);
			inc(ans, int64(d) * res[l].b);
			inc(r, d);
			inc(l);
		end;

		inc(ans, int64(n-r) * p);

		writeln(ans);

    end;
end.

```
