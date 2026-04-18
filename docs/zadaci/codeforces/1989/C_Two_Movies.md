# Задатак: C_Two_Movies.pas

```pascal
program C_Two_Movies;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, x, y, mn: int32;
    r: int8;
    a, b: array [1 .. nn] of int8;
    c: array [-1 .. 1] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(b[i]); readln;

        x := 0;
        y := 0;
        for r := -1 to 1 do c[r] := 0;

        for i := 1 to n do
            if a[i] > b[i] then
                inc(x, a[i])
            else if b[i] > a[i] then
                inc(y, b[i])
            else
                inc(c[a[i]]);

        if x > y then begin

            mn := min(x-y, c[1]);
            inc(y, mn);
            dec(c[1], mn);

            mn := min(x-y, c[-1]);
            dec(x, mn);
            dec(c[-1], mn);

        end else if y > x then begin

            mn := min(y-x, c[1]);
            inc(x, mn);
            dec(c[1], mn);

            mn := min(y-x, c[-1]);
            dec(y, mn);
            dec(c[-1], mn);

        end;

        inc(x, c[1] div 2);
        inc(y, (c[1]+1) div 2);
        dec(x, c[-1] div 2);
        dec(y, (c[-1]+1) div 2);

        writeln(min(x, y));

    end;
end.

```
