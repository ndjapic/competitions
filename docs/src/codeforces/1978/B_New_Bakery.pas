program B_New_Bakery;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    n, a, b, k: int32;
    x: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, a, b);

        k := max(0, b-a);
        k := min(n, k);
        x := int64(b+b-k+1) * k div 2 + int64(n-k) * a;
        writeln(x);

    end;
end.
