# Problem: A_Equally.pas

```pascal
program A_Equally;
var
    a, b, c, s: int16;

begin
    readln(a, b, c);
    s := a+b+c;

    if (a = b) and (b = c) then
        writeln('Yes')
    else if (s = 2*a) or (s = 2*b) or (s = 2*c) then
        writeln('Yes')
    else
        writeln('No');
end.

```
