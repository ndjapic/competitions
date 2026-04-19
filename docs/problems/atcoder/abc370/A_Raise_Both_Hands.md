# Problem: A_Raise_Both_Hands.pas

```pascal
program A_Raise_Both_Hands;
var
    l, r, x: int8;

begin
    readln(l, r);
    x := l*2+r;

    if x = 2 then
        writeln('Yes')
    else if x = 1 then
        writeln('No')
    else
        writeln('Invalid');
end.

```
