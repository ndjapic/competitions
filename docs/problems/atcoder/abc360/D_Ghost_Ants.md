# Problem: D_Ghost_Ants.pas

```pascal
program D_Ghost_Ants;
{$mode objfpc}{$H+}{$J-}
const
    nn = 200 * 1000;
var
    n, t, i, j: int32;
    ans: int64;
    a, merge: array [1 .. nn] of record
        s: char;
        x: int32;
    end;
    c: array [0 .. nn] of int32;

procedure msort(lend, rend: int32);
var
    i, l, r, m: int32;
begin
    if rend - lend > 1 then begin

        m := (lend + rend) div 2;
        msort(lend, m);
        msort(m, rend);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and (a[l].x <= a[r].x) then begin
                merge[i] := a[l];
                inc(l);
            end else begin
                merge[i] := a[r];
                inc(r);
            end;

        for i := lend to rend - 1 do a[i] := merge[i];

    end;
end;

begin
    readln(n, t);

    for i := 1 to n do read(a[i].s); readln;
    for i := 1 to n do read(a[i].x); readln;
    msort(1, n+1);

    c[0] := 0;
    for i := 1 to n do begin
        c[i] := c[i-1];
        if a[i].s = '1' then inc(c[i]);
    end;

    ans := 0;
    i := 1;
    for j := 1 to n do
        if a[j].s = '0' then begin
            while (a[i].x + t < a[j].x - t) do inc(i);
            inc(ans, c[j-1] - c[i-1]);
        end;

    writeln(ans);
end.

```
