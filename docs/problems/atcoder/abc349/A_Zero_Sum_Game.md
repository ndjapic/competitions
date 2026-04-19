# Problem: A_Zero_Sum_Game.pas

```pascal
program A_Zero_Sum_Game;
const
    maxn = 100;
var
    n, i, x: int8;
    s: int16;

begin
    readln(n);

    s := 0;
    for i := 1 to n-1 do begin
        read(x);
        inc(s, x);
    end;
    readln;

    writeln(-s);
end.

```
