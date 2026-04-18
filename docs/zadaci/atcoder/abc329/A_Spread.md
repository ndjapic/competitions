# Задатак: A_Spread.pas

```pascal
program A_Spread;
uses
    math;
const
    maxn = 100;
var
    n, i: int16;
    s: array [1 .. maxn] of char;

begin
    n := 0;
    repeat
        inc(n);
        read(s[n]);
    until eoln;
    readln;

    for i := 1 to n-1 do write(s[i], ' '); writeln(s[n]);
end.

```
