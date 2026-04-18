# Задатак: D_GCD_sequence.pas

```pascal
program D_GCD_sequence;
{$mode objfpc}{$H+}{$J-}
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, c: int32;
    a, b: array [1 .. nn] of int32;

function gcd(x, y: int32): int32;
begin
    if y = 0 then
        result := x
    else
        result := gcd(y, x mod y);
end;

function canRemove(i, c: int32): boolean;
var
    g: int32;
begin
    if (i-2 >= 1) and (b[i-2] > b[i-1]) then dec(c);
    if (i-1 >= 1) and (i <= n-1) and (b[i-1] > b[i]) then dec(c);
    if (i+1 <= n-1) and (b[i] > b[i+1]) then dec(c);
    if (i-1 >= 1) and (i+1 <= n) then begin
        g := gcd(a[i-1], a[i+1]);
        if (i-2 >= 1) and (b[i-2] > g) then inc(c);
        if (i+1 <= n-1) and (g > b[i+1]) then inc(c);
    end;
    result := c = 0;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        c := 0;

        for i := 1 to n do begin
            read(a[i]);
            if i > 1 then b[i-1] := gcd(a[i-1], a[i]);
            if (i > 2) and (b[i-2] > b[i-1]) then inc(c);
        end;
        readln;

        i := 1;
        while (i <= n) and not canRemove(i, c) do inc(i);

        if i <= n then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
