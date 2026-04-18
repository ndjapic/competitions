# Задатак: C_Festival.pas

```pascal
program C_Festival;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, m, i, j: int32;
    a, b: array [0 .. maxn] of int32;

begin
    readln(n, m);
    a[0] := 0;
    for j := 1 to m do read(a[j]); readln;

    j := m;
    for i := n downto 1 do
        if a[j] = i then begin
            b[i] := 0;
            dec(j);
        end else
            b[i] := b[i+1] + 1;

    for i := 1 to n do writeln(b[i]);
end.


```
