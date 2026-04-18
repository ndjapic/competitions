# Задатак: C_Popcorn.pas

```pascal
program C_Popcorn;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 10;
var
    n, m, i, e, ans: int8;
    x, mask: int16;
    s: string;
    flavors: array [0 .. nn] of int16;
    nobs: array [0 .. 1023] of int8;

begin
    nobs[0] := 0;
    for mask := 0 to 511 do begin
        nobs[2 * mask] := nobs[mask];
        nobs[2 * mask + 1] := nobs[mask] + 1;
    end;

    readln(n, m);

    for i := 0 to n-1 do begin
        readln(s);
        flavors[i] := 0;
        for e := 0 to m-1 do
            if s[e+1] = 'o' then
                inc(flavors[i], int16(1) shl e);
    end;

    ans := n;
    for mask := 0 to (int16(1) shl n) - 1 do begin
        x := 0;
        for i := 0 to n-1 do
            if odd(mask shr i) then
                x := x or flavors[i];
        if x = (int16(1) shl m) - 1 then ans := min(ans, nobs[mask]);
    end;
    writeln(ans);
end.

```
