# Problem: D_Palindromic_Number.pas

```pascal
program D_Palindromic_Number;
{$mode objfpc}{$H+}{$J-}
var
    n, c, x: int64;
    d, h, e, i: int8;
    pal: array [0 .. 18] of int8;

begin
    readln(n);

    if n = 1 then
        writeln(0)
    else begin

        c := 1;
        d := 0;
        while n > c do begin
            dec(n, c);
            inc(d);
            if d = 1 then
                c := 9
            else if odd(d) then
                c := c * 10;
        end;

        x := 1;
        h := (d+1) div 2;
        for e := 1 to h-1 do x := x * 10;
        inc(x, n-1);

        e := 0;
        while x > 0 do begin
            inc(e);
            pal[e] := x mod 10;
            x := x div 10;
        end;

        for i := e downto 1 do write(pal[i]);
        for i := 1 + d mod 2 to e do write(pal[i]);
        writeln;

    end;
end.

```
