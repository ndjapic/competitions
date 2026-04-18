# Задатак: A_Subsegment_Reverse.pas

```pascal
program A_Subsegment_Reverse;
const
    nn = 100;
var
    n, i, l, r: int32;
    a: array [1 .. nn] of int8;

begin
    readln(n, l, r);
    for i := 1 to n do a[i] := i;
    for i := l to r do a[i] := l+r-i;
    for i := 1 to n-1 do write(a[i], ' '); writeln(a[n]);
end.

```
