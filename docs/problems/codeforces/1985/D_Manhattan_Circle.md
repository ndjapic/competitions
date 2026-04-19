# Problem: D_Manhattan_Circle.pas

```pascal
program D_Manhattan_Circle;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int16;
    n, m, i, h, k, l, r, d: int32;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        d := 0;
        for i := 1 to n do begin

            readln(s);

            l := 1;
            r := m;
            while (l <= r) and (s[r] = '.') do dec(r);
            while (l <= r) and (s[l] = '.') do inc(l);

            if r-l+1 > d then begin
                d := r-l+1;
                h := i;
                k := (l+r) div 2;
            end;

        end;

        writeln(h, ' ', k);

    end;
end.

```
