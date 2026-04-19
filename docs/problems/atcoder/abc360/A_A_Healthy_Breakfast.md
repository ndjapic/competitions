# Problem: A_A_Healthy_Breakfast.pas

```pascal
program A_A_Healthy_Breakfast;
{$mode objfpc}{$H+}{$J-}
var
    s: string;

begin
    readln(s);

    if (s[1] = 'R') or (s[2] = 'R') and (s[3] = 'M') then
        writeln('YES')
    else
        writeln('NO');
end.

```
