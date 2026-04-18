# Задатак: A_Yogurt_Sale.pas

```pascal
program A_Yogurt_Sale;
uses
    math;
var
    ntc, tci: int16;
    n, a, b: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, a, b);

        writeln(
            min(
                n*a,
                n div 2 * b + n mod 2 * a
            )
        );

    end;
end.

```
