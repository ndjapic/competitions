# Problem: C_Boring_Day.pas

```pascal
program C_Boring_Day;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, l, r, i, j, x, ans: int32;
    a: array [0 .. nn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, l, r);

        a[0] := 0;
        j := 0;
        ans := 0;

        for i := 1 to n do begin

            read(x);
            a[i] := a[i-1] + x;

            while a[i] - a[j] > r do inc(j);
            if a[i] - a[j] >= l then begin
                inc(ans);
                j := i;
            end;

        end;
        readln;

        writeln(ans);

    end;
end.

```
