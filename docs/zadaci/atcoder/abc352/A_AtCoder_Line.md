# Задатак: A_AtCoder_Line.pas

```pascal
program A_AtCoder_Line;
var
    n, x, y, z: int8;

begin
    readln(n, x, y, z);
    if (x < z) and (z < y) or (y < z) and (z < x) then
        writeln('Yes')
    else
        writeln('No');
end.

```
