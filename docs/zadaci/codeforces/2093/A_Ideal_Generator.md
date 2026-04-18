# Задатак: A_Ideal_Generator.pas

```pascal
program A_Ideal_Generator;
var
    ntc, tci: int16;
    k: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(k);

        if odd(k) then
            writeln('YES')
        else
            writeln('NO');

    end;

end.

```
