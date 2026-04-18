# Задатак: B_Bought_Review.pas

```pascal
program B_Bought_Review;
uses
    math;
var
    ntc, tci: int16;
    i: int8;
    d, ans: int64;
    a, p: array [1 .. 5] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        for i := 1 to 5 do read(a[i]); readln;
        for i := 1 to 5 do read(p[i]); readln;

        d := max(0, a[2] - a[4] + 2 * (a[1] - a[5]));

        ans := min(
            d * p[4],
            (d+1) div 2 * p[5]
        );

        ans := min(ans,
            p[4] + d div 2 * p[5]
        );

        writeln(ans);

    end;
end.

```
