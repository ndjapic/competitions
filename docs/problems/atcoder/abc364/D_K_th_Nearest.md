# Problem: D_K_th_Nearest.pas

```pascal
program D_K_th_Nearest;
{$mode objfpc}{$H+}{$J-}
const
    nn = 100 * 1000;
    inf = 1000 * 1000 * 1000;
var
    n, q, i, j, b, k, l, r, m: int32;
    a, merge: array [1 .. nn] of int32;

procedure msort(lend, rend: int32);
var
    i, l, r, m: int32;

    function LessOrEqual(l, r: int32): boolean;
    begin
        result := a[l] <= a[r];
    end;

begin
    i := lend + 1;
    while (i < rend) and LessOrEqual(i-1, i) do inc(i);

    if i < rend then begin

        m := (lend + rend) div 2;
        if i < m then msort(lend, m);
        msort(m, rend);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and LessOrEqual(l, r) then begin
                merge[i] := a[l];
                inc(l);
            end else begin
                merge[i] := a[r];
                inc(r);
            end;

        for i := lend to rend - 1 do a[i] := merge[i];

    end;
end;

function bisectr(x: int32): int32;
var
    l, r, m: int32;
begin
    l := 0;
    r := n+1;
    while r-l > 1 do begin
        m := (l+r) div 2;
        if x < a[m] then
            r := m
        else
            l := m;
    end;
    bisectr := r;
end;

begin
    readln(n, q);
    for i := 1 to n do read(a[i]); readln; msort(1, n+1);

    for j := 1 to q do begin
        readln(b, k);

        l := -1;
        r := inf;
        while r-l > 1 do begin
            m := (l+r) div 2;
            if bisectr(b+m) - bisectr(b-m-1) >= k then
                r := m
            else
                l := m;
        end;
        writeln(r);
    end;
end.

```
