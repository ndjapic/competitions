# Задатак: E_Data_Structures_Fan.pas

```pascal
program E_Data_Structures_Fan;
uses
	math;
const
	maxn = 100 * 1000;
	prime = 998244353;
var
    n, i, j, tot, act, ai, ans: int32;
    e: int16;
    a, p, cp, t: array [1 .. maxn] of int32;
    pow2: array [0 .. maxn] of int32;
    d: array [1 .. maxn] of array of int32;

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
            if (ir > r) or (il <= m) and (a[p[il]] >= a[p[ir]]) then begin
                cp[i] := p[il];
                inc(il);
            end else begin
                cp[i] := p[ir];
                inc(ir);
            end;

        for i := l to r do p[i] := cp[i];

    end;
end;

begin
	pow2[0] := 1;
	for n := 1 to maxn do begin
		setlength(d[n], 2);
		t[n] := 0;
		pow2[n] := pow2[n-1] * 2 mod prime;
	end;

	for i := 1 to maxn do begin
		n := i;
		while n <= maxn do begin
			if length(d[n]) = t[n] then setlength(d[n], 2*t[n]);
			d[n][t[n]] := i;
			inc(t[n]);
			inc(n, i);
		end;
	end;

	readln(n);

	for i := 1 to n do begin
		read(a[i]);
		p[i] := i;
	end;
	readln;
	msort(1, n);

	ans := 0;
	tot := n;

	for j := 1 to n do begin

		i := p[j];
		ai := a[i];

		if ai > -1 then begin
			act := 0;

			for e := 0 to t[i]-1 do
				if a[d[i][e]] > -1 then begin
					inc(act);
					a[d[i][e]] := -1;
				end;

			ans := (ans + int64(pow2[tot] - pow2[tot-act] + prime) * ai) mod prime;
			dec(tot, act);
		end;

	end;

	writeln(ans);
end.

```
