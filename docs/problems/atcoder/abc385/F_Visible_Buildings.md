# Problem: F_Visible_Buildings.pas

```pascal
program F_Visible_Buildings;
uses
    math;
const
    nn = 200 * 1000;
    eps = 1e-18;
var
    n, i: int32;
    mx: extended;
    x, h: array [1 .. nn] of int64;

begin
    readln(n);
    readln(x[1], h[1]);

    mx := -eps;
    for i := 2 to n do begin
        readln(x[i], h[i]);
        mx := max(mx, (h[i-1]*x[i] - h[i]*x[i-1]) / (x[i] - x[i-1]));
    end;

    if mx < 0 then
        writeln(-1)
    else
        writeln(mx:35:18);
end.

```
