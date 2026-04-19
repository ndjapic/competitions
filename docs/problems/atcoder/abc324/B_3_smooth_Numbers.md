# Problem: B_3_smooth_Numbers.pas

```pascal
program B_3_smooth_Numbers;
var
    n: int64;

begin
    readln(n);

    while n mod 2 = 0 do n := n div 2;
    while n mod 3 = 0 do n := n div 3;

    if n = 1 then
        writeln('Yes')
    else
        writeln('No');
end.

```
