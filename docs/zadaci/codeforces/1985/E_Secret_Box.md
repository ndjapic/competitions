# Задатак: E_Secret_Box.pas

```pascal
program E_Secret_Box;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    x, y, z, k, v, ans: int64;
    a, b, c: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x, y, z, k);

        ans := 0;
        a := 1;
        while a <= x do begin
            if k mod a = 0 then begin
                v := k div a;
                b := 1;
                while b <= y do begin
                    if v mod b = 0 then begin
                        c := v div b;
                        if c <= z then
                            ans := max(ans, int64(x-a+1) * (y-b+1) * (z-c+1));
                    end;
                    inc(b);
                end;
            end;
            inc(a);
        end;

        writeln(ans);

    end;
end.

```
