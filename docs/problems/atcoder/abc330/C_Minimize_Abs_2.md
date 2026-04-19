# Problem: C_Minimize_Abs_2.pas

```pascal
program C_Minimize_Abs_2;
uses
    math;
var
    x, y, d, dif, ans: int64;

function isqrt(a: int64): int64;
var
    x: int64;
begin
    x := min(a, high(int32));
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

begin
    readln(d);

    x := isqrt(d*2)+1;
    y := 0;
    ans := high(int64);
    while (x >= 0) and (ans > 0) do begin
        dif := x*x+y*y-d;
        ans := min(ans, abs(dif));
        if dif > 0 then
            dec(x)
        else
            inc(y);
    end;

    writeln(ans);
end.

```
