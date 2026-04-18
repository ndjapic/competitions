# Задатак: C_Matching_Arrays.pas

```pascal
program C_Matching_Arrays;
uses
    math;
const
    maxn = 200 * 1000;
type
    tarray = array [1 .. maxn] of int32;
var
    ntc, tci: int16;
    n, x, i: int32;
    ans: boolean;
    a, b, p, q, z, merge: tarray;

procedure msort(var c, t: tarray; l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(c, t, l, m);
        msort(c, t, m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (c[t[il]] <= c[t[ir]]) then begin
                merge[i] := t[il];
                inc(il);
            end else begin
                merge[i] := t[ir];
                inc(ir);
            end;

        for i := l to r do t[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);

        for i := 1 to n do begin
            read(a[i]);
            p[i] := i;
        end;
        readln;
        msort(a, p, 1, n);

        for i := 1 to n do begin
            read(b[i]);
            q[i] := i;
        end;
        readln;
        msort(b, q, 1, n);

        i := 1;
        while (i <= x) and (a[p[n-x+i]] > b[q[i]]) do begin
            z[p[n-x+i]] := b[q[i]];
            inc(i);
        end;
        ans := i > x;

        if ans then begin
            i := 1;
            while (i <= n-x) and (a[p[i]] <= b[q[x+i]]) do begin
                z[p[i]] := b[q[x+i]];
                inc(i);
            end;
            ans := i > n-x;
        end;

        if ans then begin

            writeln('YES');
            for i := 1 to n-1 do write(z[i], ' ');
            writeln(z[n]);

        end else
            writeln('NO');

    end;
end.

```
