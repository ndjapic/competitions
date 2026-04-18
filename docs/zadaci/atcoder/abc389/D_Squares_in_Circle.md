# Задатак: D_Squares_in_Circle.pas

```pascal
program D_Squares_in_Circle;
uses
    math;
var
    i: int32;
    r, x, y, ans: int64;

begin
    readln(r);

    ans := 0;
    y := 2*r;
    for i := 1 to r do begin
        x := 2*i - 1;
        while x*x + y*y > r*r*4 do dec(y);
        inc(ans, (y-1) div 2);
    end;

    writeln(4*ans + 1);
end.

```
