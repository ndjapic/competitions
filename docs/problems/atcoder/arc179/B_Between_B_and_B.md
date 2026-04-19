# Problem: B_Between_B_and_B.pas

```pascal
program B_Between_B_and_B;
const
    mm = 10;
    nn = 200 * 1000;
var
    m, b: int8;
    n, i: int32;
    x: array [1 .. mm] of int8;
    a: array [1 .. nn] of int8;

begin
    readln(m, n);
    for b := 1 to m do read(x[b]);
    readln;

end.

```
