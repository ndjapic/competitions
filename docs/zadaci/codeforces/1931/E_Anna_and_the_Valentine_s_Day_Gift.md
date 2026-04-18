# Задатак: E_Anna_and_the_Valentine_s_Day_Gift.pas

```pascal
program E_Anna_and_the_Valentine_s_Day_Gift;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, m, i, x, digits: int32;
    e: int8;
    c: array [0 .. 9] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for e := 0 to 9 do c[e] := 0;
        digits := 0;

        for i := 1 to n do begin

            read(x);
            e := 0;

            while x mod 10 = 0 do begin
                x := x div 10;
                inc(e);
            end;
            inc(c[e]);

            while x > 0 do begin
                x := x div 10;
                inc(e);
            end;
            inc(digits, e);

        end;
        readln;

        e := 9;
        for i := 1 to n do begin
            while c[e] = 0 do dec(e);
            if odd(i) then dec(digits, e);
            dec(c[e]);
        end;

        if digits <= m then
            writeln('Anna')
        else
            writeln('Sasha');

    end;
end.

```
