# Задатак: A_Soccer.pas

```pascal
program A_Soccer;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int16;
    x1, y1, x2, y2: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x1, y1);
        readln(x2, y2);

        if (x1 < y1) = (x2 < y2) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
