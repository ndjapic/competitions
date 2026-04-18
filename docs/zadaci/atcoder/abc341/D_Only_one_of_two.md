# Задатак: D_Only_one_of_two.pas

```pascal
program D_Only_one_of_two;
var
    n, m, k, d, l, r, x: int64;

function gcd(a, b: int64): int64;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

function lcm(a, b: int64): int64;
begin
    lcm := a div gcd(a, b) * b;
end;

begin
    readln(n, m, k);

    d := lcm(n, m);

    l := 0;
    r := 1000 * 1000 * 1000;
    r := r * r + 1;
    while r-l > 1 do begin
        x := (l+r) div 2;
        if k > x div n - x div d * 2 + x div m then
            l := x
        else
            r := x;
    end;

    writeln(r);
end.

```
