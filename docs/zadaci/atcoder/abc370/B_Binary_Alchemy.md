# Задатак: B_Binary_Alchemy.pas

```pascal
program B_Binary_Alchemy;
const
    nn = 100;
var
    n, i, j: int8;
    a: array [1 .. nn, 1 .. nn] of int8;

begin
    readln(n);

    for i := 1 to n do begin
        for j := 1 to i do read(a[i, j]); readln;
    end;

    i := 1;
    for j := 1 to n do
        if i >= j then
            i := a[i, j]
        else
            i := a[j, i];

    writeln(i);
end.

```
