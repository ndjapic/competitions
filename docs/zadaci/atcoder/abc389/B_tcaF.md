# Задатак: B_tcaF.pas

```pascal
program B_tcaF;
const
    nn = 32;
var
    x, n: int64;
    fact: array [0 .. nn] of int64;

begin
    fact[0] := 1;
    for n := 1 to nn do fact[n] := n * fact[n-1];

    readln(x);

    n := 0;
    while fact[n] < x do inc(n);

    writeln(n);
end.

```
