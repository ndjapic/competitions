# Задатак: C_Serval_and_The_Formula.pas

```pascal
program C_Serval_and_The_Formula;
const
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int32;
    x, y: int64;
    k, p2: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x, y);

        p2 := 1;
        while (p2 <= inf) and ((x and p2 = 0) or (y and p2 = 0)) do
            inc(p2, p2);

        if p2 > inf then
            k := 0
        else
            k := p2 div 2;

        inc(x, k);
        inc(y, k);
        if x+y <> x xor y then k := -1;

        writeln(k);

    end;
end.

```
