# Задатак: B_Make_Almost_Equal_With_Mod.pas

```pascal
program B_Make_Almost_Equal_With_Mod;
uses
    math;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, i: int8;
    k: int64;
    a: array [1 .. maxn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        k := 1;
        repeat
            inc(k, k);
            i := 1;
            while (i < n) and ( (a[i] xor a[n]) mod k = 0 ) do inc(i);
        until i < n;

        writeln(k);

    end;
end.

```
