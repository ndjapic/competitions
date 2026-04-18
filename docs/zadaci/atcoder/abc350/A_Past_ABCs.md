# Задатак: A_Past_ABCs.pas

```pascal
program A_Past_ABCs;
{$H+}
var
    s: string;
    n: int16;
    i: int8;

begin
    readln(s);

    n := 0;
    for i := 4 to 6 do
        n := 10 * n + ord(s[i]) - ord('0');

    if n = 0 then
        writeln('No')
    else if n = 316 then
        writeln('No')
    else if n > 349 then
        writeln('No')
    else
        writeln('Yes');
end.

```
