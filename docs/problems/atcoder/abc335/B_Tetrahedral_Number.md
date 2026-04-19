# Problem: B_Tetrahedral_Number.pas

```pascal
program B_Tetrahedral_Number;
var
    n, x, y, z: int8;

begin
    readln(n);

    for x := 0 to n do
        for y := 0 to n-x do
            for z := 0 to n-x-y do
                writeln(x, ' ', y, ' ', z);
end.

```
