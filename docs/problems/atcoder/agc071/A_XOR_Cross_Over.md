# Problem: A_XOR_Cross_Over.pas

```pascal
program A_XOR_Cross_Over;
const
    nn = 500;
var
    n, i, x, ans: int32;
    a: array [1 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n do begin
        read(x);
        a[i] := x;
    end;
    readln;
end.

```
