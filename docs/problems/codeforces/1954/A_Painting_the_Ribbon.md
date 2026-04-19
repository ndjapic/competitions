# Problem: A_Painting_the_Ribbon.pas

```pascal
program A_Painting_the_Ribbon;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int16;
    n, m, k: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

        if (n+m-1) div m + k < n then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
