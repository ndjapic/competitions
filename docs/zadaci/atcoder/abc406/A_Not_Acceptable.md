# Задатак: A_Not_Acceptable.pas

```pascal
program A_Not_Acceptable;
var
    a, b, c, d: int32;

begin
    readln(a, b, c, d);

    inc(b, a*60);
    inc(d, c*60);

    if d > b then
        writeln('No')
    else
        writeln('Yes');
end.

```
