# Задатак: B_Coin_Games.pas

```pascal
program B_Coin_Games;
{$H+}
const
    sz = 100;
var
    ntc, tci: int8;
    n, i, c: int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        c := 0;
        for i := 1 to n do
            if s[i] = 'U' then inc(c);

        if odd(c) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
