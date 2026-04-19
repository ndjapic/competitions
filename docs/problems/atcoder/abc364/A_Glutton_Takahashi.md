# Problem: A_Glutton_Takahashi.pas

```pascal
program A_Glutton_Takahashi;
{$mode objfpc}{$H+}{$J-}
const
    nn = 100;
var
    n, i: int8;
    s: array [1 .. nn] of string;

begin
    readln(n);
    for i := 1 to n do readln(s[i]);

    i := 2;
    while (i <= n) and not (
        (s[i-1][2] = 'w') and (s[i][2] = 'w')
    ) do inc(i);

    if i < n then
        writeln('No')
    else
        writeln('Yes');
end.

```
