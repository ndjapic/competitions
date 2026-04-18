# Задатак: A_Kevin_and Arithmetic.pas

```pascal
program A_Kevin_and_Arithmetic;
uses
    math;
const
    nn = 100;
var
    ntc, tci: int16;
    n, i, ans: int8;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        ans := 0;
        for i := 1 to n do begin
            read(a[i]);
            inc(ans, a[i] mod 2);
        end;
        readln;

        if ans = n then
            dec(ans)
        else if ans = 0 then
            inc(ans)
        else
            inc(ans);

        writeln(ans);

    end;
end.

```
