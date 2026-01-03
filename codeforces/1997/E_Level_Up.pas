program E_Level_Up;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, q, i, j, x, l, k, c: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, q);

        for i := 1 to n do read(a[i]); readln;

        for j := 1 to q do begin

            readln(i, x);
            l := 1;
            c := 0;

            for k := 1 to i-1 do
                if a[k] >= l then

        end;

        writeln(cost);

    end;
end.
