# Problem: B_Collatz_Conjecture.pas

```pascal
program B_Collatz_Conjecture;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    x, y, k, d: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x, y, k);

        while (k > 0) and (x > 1) do begin
            d := min(k, x div y * y + y - x);
            inc(x, d);
            while x mod y = 0 do x := x div y;
            dec(k, d);
        end;

        k := k mod (y-1);

        writeln(x+k);

    end;
end.

```
