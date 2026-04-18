# Задатак: C_Light_Switches.pas

```pascal
program C_Light_Switches;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, k, i, mx, l, r, p: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        mx := 0;
        p := 2*k;

        for i := 1 to n do begin
            read(a[i]);
            mx := max(mx, a[i]);
        end;
        readln;

        l := mx;
        r := mx + k-1;

        for i := 1 to n do begin
            inc(a[i], (mx-a[i] + k) div p * p);
            l := max(l, a[i]);
            r := min(r, a[i] + k-1);
        end;

        if l > r then l := -1;
        writeln(l);

    end;
end.

```
