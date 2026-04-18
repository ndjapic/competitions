# Задатак: B_Intesection_of_Cuboids.pas

```pascal
program B_Intesection_of_Cuboids;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    a, b, c, d, e, f, g, h, i, j, k, l: int16;
    ans: boolean;

begin
    readln(a, b, c, d, e, f);
    readln(g, h, i, j, k, l);

    ans := true;
    ans := ans and (max(a, g) < min(d, j));
    ans := ans and (max(b, h) < min(e, k));
    ans := ans and (max(c, i) < min(f, l));

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.

```
