# Задатак: D_1D_Country.pas

```pascal
program D_1D_Country;
uses
    math;
const
    nn = 200 * 1000;
var
    n, q, i, l, r: int32;
    x: array [1 .. nn] of int32;
    p: array [0 .. nn] of int64;

function rank(y: int32): int32;
var
    l, r, m: int32;
begin
    l := 0;
    r := n+1;
    while r-l > 1 do begin
        m := (l+r) div 2;
        if y < x[m] then
            r := m
        else
            l := m;
    end;
    rank := l;
end;

begin
    readln(n);

    for i := 1 to n do read(x[i]); readln;

    p[0] := 0;
    for i := 1 to n do begin
        read(p[i]);
        inc(p[i], p[i-1]);
    end;
    readln;

    readln(q);
    for i := 1 to q do begin
        readln(l, r);
        writeln(p[rank(r)] - p[rank(l-1)]);
    end;
end.

```
