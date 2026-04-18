# Задатак: B_Humidifier_2.pas

```pascal
program B_Humidifier_2;
{$mode delphi}
uses
    math;
const
    nn = 100;
var
    h, w, d, n, i, j, h1, h2, c, humidified, mx: int8;
    s: string;
    f: array [1 .. nn] of record
        i, j: int8;
    end;

function dist(i1, j1, i2, j2: int8): int8;
begin
    dist := abs(i1-i2) + abs(j1-j2);
end;

begin
    readln(h, w, d);

    n := 0;
    for i := 1 to h do begin
        readln(s);
        for j := 1 to w do
            if s[j] = '.' then begin
                inc(n);
                f[n].i := i;
                f[n].j := j;
            end;
    end;

    mx := 0;
    for h1 := 1 to n-1 do
        for h2 := h1+1 to n do begin
            humidified := 0;
            for c := 1 to n do
                if
                    (dist(f[h1].i, f[h1].j, f[c].i, f[c].j) <= d) or
                    (dist(f[h2].i, f[h2].j, f[c].i, f[c].j) <= d)
                then
                    inc(humidified);
            mx := max(mx, humidified);
        end;

    writeln(mx);
end.

```
