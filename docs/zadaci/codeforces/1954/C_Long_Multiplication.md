# Задатак: C_Long_Multiplication.pas

```pascal
program C_Long_Multiplication;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 300 * 1000;
var
    ntc, tci: int16;
    x, y: string;
    ch: char;
    n, i: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x);
        readln(y);
        n := length(x);

        i := 1;
        while (i <= n) and (x[i] = y[i]) do inc(i);

        if (i <= n) and (x[i] < y[i]) then begin
            ch := x[i];
            x[i] := y[i];
            y[i] := ch;
        end;

        inc(i);
        while i <= n do begin

            if (i <= n) and (x[i] > y[i]) then begin
                ch := x[i];
                x[i] := y[i];
                y[i] := ch;
            end;

            inc(i);
        end;

        writeln(x);
        writeln(y);

    end;
end.

```
