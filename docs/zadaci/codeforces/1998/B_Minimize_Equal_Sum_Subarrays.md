# Задатак: B_Minimize_Equal_Sum_Subarrays.pas

```pascal
program B_Minimize_Equal_Sum_Subarrays;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int32;
    n, i: int32;
    p, q: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do begin
            read(p[i]);
            q[i] := p[i] mod n + 1;
        end;
        readln;

        for i := 1 to n-1 do write(q[i], ' ');
        writeln(q[n]);

    end;
end.

```
