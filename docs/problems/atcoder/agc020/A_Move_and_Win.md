# Problem: A_Move_and_Win.pas

```pascal
program A_Move_and_Win;
var
    n, a, b: int8;

begin
    readln(n, a, b);

    if odd(a-b) then
        writeln('Borys')
    else
        writeln('Alice');
end.

```
