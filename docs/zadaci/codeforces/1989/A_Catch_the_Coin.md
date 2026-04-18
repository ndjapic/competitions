# Задатак: A_Catch_the_Coin.pas

```pascal
program A_Catch_the_Coin;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int16;
    x, y: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x, y);

        if y >= -1 then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
