# Задатак: euler099.pas

```pascal
program Largest_exponential;
uses
    math;
const
    maxn = 100 * 1000;
var
    n, k, i: int32;
    p, merge: array [1 .. maxn] of int32;
    a: array [1 .. maxn] of record
        b, e: int32;
        x: extended;
    end;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                a[p[j]].x <= a[p[k]].x
            ) then begin
                merge[i] := p[j];
                inc(j);
            end else begin
                merge[i] := p[k];
                inc(k);
            end;

        for i := l to r-1 do p[i] := merge[i];

    end;
end;

begin
    readln(n);

    for i := 1 to n do begin
        readln(a[i].b, a[i].e);
        a[i].x := ln(extended(a[i].b)) * a[i].e;
        p[i] := i;
    end;
    msort(1, n+1);

    readln(k);
    writeln(a[p[k]].b, ' ', a[p[k]].e);
end.

```
