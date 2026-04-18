# Задатак: C_Can_I_Square.pas

```pascal
program C_Can_I_Square;
uses
    math;
var
    ntc, tci: int16;
    n, i, ai: int32;
    s, x: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        s := 0;

        for i := 1 to n do begin

            read(ai);
            inc(s, ai);

        end;
        readln;

        x := min(s, high(int32));
        while x*x > s do
            x := (x + s div x) div 2;

        if x*x = s then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
