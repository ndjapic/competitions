# Задатак: B_The_Picky_Cat.pas

```pascal
program B_The_Picky_Cat;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, h, c: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        h := (n-1) div 2;

        c := 0;
        for i := 1 to n do begin
            read(a[i]);
            if abs(a[i]) > abs(a[1]) then inc(c);
        end;
        readln;

        if c >= h then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
