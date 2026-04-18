# Задатак: A_Little_Nikita.pas

```pascal
program A_Little_Nikita;
var
    ntc, tci: int8;
    n, m: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        if m > n then
            writeln('No')
        else if odd(n-m) then
            writeln('No')
        else
            writeln('Yes');

    end;
end.

```
