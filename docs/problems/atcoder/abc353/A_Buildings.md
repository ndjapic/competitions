# Problem: A_Buildings.pas

```pascal
program E_Clique_Connect;
uses
    math;
const
    nn = 100;
var
    n, i: int32;
    h: array [1 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n do read(h[i]);
    readln;

    i := 2;
    while (i <= n) and (h[i] <= h[1]) do inc(i);

    if i > n then i := -1;
    writeln(i);
end.

```
