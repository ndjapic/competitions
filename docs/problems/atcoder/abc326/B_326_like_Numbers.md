# Problem: B_326_like_Numbers.pas

```pascal
program B_326_like_Numbers;
var
    n: int16;
    a, b, c: int8;

begin
    readln(n);
    dec(n);

    repeat
        inc(n);
        a := n div 100;
        b := n div 10 mod 10;
        c := n mod 10;
    until a * b = c;

    writeln(n);
end.

```
