# Задатак: A_Fashionable_Array.pas

```pascal
program A_Fashionable_Array;
uses
    math;
const
    nn = 50;
var
    ntc, tci: int16;
    n, i, x, l, r: int8;
    a, c: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for x := 1 to 50 do c[x] := 0;

        for i := 1 to n do begin
            read(x);
            inc(c[x]);
        end;
        readln;

        i := 0;
        x := 1;
        while x <= 50 do
            if c[x] = 0 then
                inc(x)
            else begin
                inc(i);
                a[i] := x;
                dec(c[x]);
            end;


        l := 1;
        r := n;
        while odd(a[l] + a[n]) do inc(l);
        while odd(a[1] + a[r]) do dec(r);

        writeln(min(l-1, n-r));

    end;
end.

```
