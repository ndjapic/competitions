# Задатак: B.pas

```pascal
program Permutation_Swap;
var
    notc: int16;
    n, i, x, g: int32;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
    readln(notc);
    repeat

        readln(n);
        g := 0;

        for i := 1 to n do begin
            read(x);
            g := gcd(g, abs(x-i));
        end;
        readln;

        writeln(g);

        dec(notc);
    until notc = 0;
end.


```
