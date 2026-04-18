# Задатак: D_Place_of_the_Olympiad.pas

```pascal
program D_Place_of_the_Olympiad;
var
    ntc, tci: int16;
    n, m, k, l, r, b: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

        l := 0;
        r := m;
        while r-l > 1 do begin
            b := (l+r) div 2;
            if int64(m - m div (b+1)) * n < k then
                l := b
            else
                r := b;
        end;

        writeln(r);

    end;
end.

```
