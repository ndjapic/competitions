# Problem: F_Athletic.pas

```pascal
program F_Athletic;
uses
    math;
const
    nn = 500 * 1000;
var
    n, d, r, i, ans: int32;
    h: array [1 .. nn] of int32;

begin
    readln(n, d, r);

    for i := 1 to n do read(h[i]); readln;

    writeln(ans);
end.

```
