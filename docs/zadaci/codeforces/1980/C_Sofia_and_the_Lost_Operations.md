# Задатак: C_Sofia_and_the_Lost_Operations.pas

```pascal
program C_Sofia_and_the_Lost_Operations;
{$mode objfpc}{$H+}{$J-}
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, m, i, j, l, r, mi: int32;
    a, b, c, d, p, f, merge: array [1 .. nn] of int32;

procedure msort(lend, rend: int32);
var
    m, i, l, r: int32;
begin
    if rend - lend > 1 then begin

        m := (lend + rend) div 2;
        msort(lend, m);
        msort(m, rend);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and (
                (b[p[l]] <= b[p[r]]) and (
                    (b[p[l]] < b[p[r]]) or
                    (f[p[l]] <= f[p[r]])
                )
            ) then begin
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

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        for i := 1 to n do begin
            read(b[i]);
            p[i] := i;
            if a[i] = b[i] then
                f[i] := 0
            else
                f[i] := 1;
        end;
        readln;
        msort(1, n+1);

        for i := 2 to n do
            if b[p[i-1]] < b[p[i]] then f[p[i]] := f[p[i-1]] + 1;

        write(' p =');
        for i := 1 to n do write(' ', p[i]);

        readln(m);
        for j := 1 to m do begin

            read(d[j]);
            c[j] := -1;

            l := 1;
            r := n+1;
            while r-l > 1 do begin
                mi := (l+r) div 2;
                if b[p[mi]] > d[j] then
                    r := mi
                else
                    l := mi;
            end;

            if b[p[l]] = d[j] then begin
                if f[p[l]] > 0 then dec(f[p[l]]);
                c[j] := p[ l - f[p[l]] ];
            end;

        end;
        readln;

        write(' c =');
        for j := 1 to m do write(' ', c[j]);

        if c[m] > -1 then
            for j := 1 to m do begin
                i := c[j];
                if i = -1 then i := c[m];
                a[i] := d[j];
            end;

        write(' c =');
        for j := 1 to m do write(' ', c[j]); writeln;

        i := 1;
        while (i <= n) and (a[i] = b[i]) do inc(i);

        if i > n then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
