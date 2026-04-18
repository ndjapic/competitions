# Задатак: B_Sasha_and_the_Drawing.pas

```pascal
program B_Sasha_and_the_Drawing;
var
    ntc, tci: int16;
    n, k: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        if k = 4*n-2 then
            writeln(2*n)
        {else if k = 4*n-3 then
            writeln(2*n-1)
        else if k = 4*n-4 then
            writeln(2*n-2)}
        else
            writeln((k+1) div 2);

    end;
end.

```
