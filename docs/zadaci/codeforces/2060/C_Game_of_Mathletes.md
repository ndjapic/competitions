# Задатак: C_Game_of_Mathletes.pas

```pascal
program C_Game_of_Mathletes;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, mn: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        read(a[1]);

        for i := 2 to n do begin
            read(a[i]);
            mn := min(a[i-1], a[i]);
            dec(a[i-1], mn);
            dec(a[i], mn);
        end;
        readln;

        i := 2;
        while (i <= n) and (a[i-1] <= a[i]) do
            inc(i);

        if i <= n then
            writeln('NO')
        else
            writeln('YES');

    end;
end.

```
