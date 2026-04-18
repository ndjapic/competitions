# Задатак: A_Exchange.pas

```pascal
program A_Exchange;
uses
    math;
const
    nn = 10;
var
    a, b, c, d, e, f, x: int16;
    n, i: int8;
    ans: boolean;

procedure pay(var y, g: int16; coin: int16);
var
    q: int16;
begin
    q := min(g, y div coin);
    dec(g, q);
    dec(y, q * coin);
end;

begin
    readln(a, b, c, d, e, f);
    readln(n);

    i := 1;
    ans := true;
    while (i <= n) and ans do begin

        read(x);

        pay(x, f, 500);
        pay(x, e, 100);
        pay(x, d, 50);
        pay(x, c, 10);
        pay(x, b, 5);
        pay(x, a, 1);

        ans := x = 0;
        inc(i);
    end;
    readln;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.

```
