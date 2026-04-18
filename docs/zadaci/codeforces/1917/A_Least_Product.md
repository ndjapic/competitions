# Задатак: A_Least_Product.pas

```pascal
program A_Least_Product;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, i, cneg, czer: int8;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        cneg := 0;
        czer := 0;

        for i := 1 to n do begin
            read(a[i]);
            if a[i] < 0 then inc(cneg);
            if a[i] = 0 then inc(czer);
        end;
        readln;

        if (czer > 0) or odd(cneg) then
            writeln(0)
        else begin

            i := 1;
            while a[i] = 0 do inc(i);
            writeln(1);
            writeln(i, ' 0');

        end;

    end;
end.

```
