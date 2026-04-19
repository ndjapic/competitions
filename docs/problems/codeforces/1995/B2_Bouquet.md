# Problem: B2_Bouquet.pas

```pascal
program B2_Bouquet;
uses
    math;
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i: int32;
    m, m1, q, q1, ans: int64;
    a0, a, merge, p, c0, c: array [0 .. nn] of int32;

procedure msort(lend, rend: int32);
var
    i, l, r, m: int32;
begin
    i := lend + 1;
    while (i < rend) and (a0[p[i-1]] <= a0[p[i]]) do inc(i);

    if i < rend then begin

        m := (lend + rend) div 2;
        if i < m then msort(lend, m);
        msort(m, rend);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and (a0[p[l]] <= a0[p[r]]) then begin
                merge[i] := p[l];
                inc(l);
            end else begin
                merge[i] := p[r];
                inc(r);
            end;

        for i := lend to rend - 1 do p[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        for i := 1 to n do read(a0[i]); readln;
        for i := 1 to n do read(c0[i]); readln;
        for i := 1 to n do p[i] := i;
        msort(1, n+1);

        ans := 0;

        for i := 1 to n do begin
            a[i] := a0[p[i]];
            c[i] := c0[p[i]];
            q := min(m div a[i], c[i]);
            ans := max(ans, q * a[i]);
        end;

        for i := 1 to n-1 do
            if a[i+1] - a[i] = 1 then begin
                q := min(m div a[i], c[i]);
                m1 := m - q * a[i];
                q1 := min(m1 div a[i+1], c[i+1]);
                ans := max(
                    ans, min(
                        q * a[i] + q1 * a[i+1] + min(q, c[i+1]-q1),
                        m
                    )
                );
            end;

        writeln(ans);

    end;
end.

```
