# Задатак: C_Vasilije_in_Cacak.pas

```pascal
program C_Vasilije_in_Cacak;
var
    ntc, tci: int16;
    n, k, x, mn, mx: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k, x);

        mn := k * (k+1) div 2;
        mx := k * (n + n-k+1) div 2;

        if (mn <= x) and (x <= mx) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
