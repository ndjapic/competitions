# Задатак: C_Sasha_and_the_Casino.pas

```pascal
program C_Sasha_and_the_Casino;
var
    ntc, tci: int16;
    k, x, i: int8;
    a, s: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(k, x, a);

        s := 0;
        for i := 1 to x+1 do
            if s <= a then
                s := ((s+1) * k - 1) div (k-1);

        if s <= a then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
