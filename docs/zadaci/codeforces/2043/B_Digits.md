# Задатак: B_Digits.pas

```pascal
program B_Digits;
var
    ntc, tci: int8;
    n: int32;
    d: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin


        readln(n, d);

        write(1);
        if (d mod 3 = 0) or (n >= 3) then write(' 3');
        if d = 5 then write(' 5');
        if (d = 7) or (n >= 3) then write(' 7');
        if (d = 9) or ((d mod 3 = 0) and (n >= 3)) or (n >= 6) then write(' 9');
        writeln();

    end;
end.

```
