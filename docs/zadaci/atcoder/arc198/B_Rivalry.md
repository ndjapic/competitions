# Задатак: B_Rivalry.pas

```pascal
program B_Rivalry;
var
    ntc, tci, x, y, z: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x, y, z);

        if x = 0 then
            writeln('No')
        else if z > x then
            writeln('No')
        else if y > x+x then
            writeln('No')
        else if odd(y) and (z = 0) then
            writeln('No')
        else
            writeln('Yes');

    end;
end.

```
