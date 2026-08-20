program A_Maximize_the_Last_Element;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    n, i, ai, x: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        x := 0;

        for i := 1 to n do begin
            read(ai);
            if odd(i) then x := max(x, ai);
        end;
        readln;

        writeln(x);

    end;
end.
