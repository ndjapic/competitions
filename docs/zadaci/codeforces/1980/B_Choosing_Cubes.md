# Задатак: B_Choosing_Cubes.pas

```pascal
program B_Choosing_Cubes;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 100;
var
    ntc, tci: int16;
    n, f, k, i, af: int8;
    a, merge: array [1 .. nn] of int8;

procedure msort(l, r: int8);
var
    m, i, j, k: int8;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                a[j] >= a[k]
            ) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, f, k);

        for i := 1 to n do read(a[i]);
        readln;
        af := a[f];
        msort(1, n+1);

        if (k = n) or (a[k+1] < af) then
            writeln('YES')
        else if a[k] = af then
            writeln('MAYBE')
        else
            writeln('NO');

    end;
end.

```
