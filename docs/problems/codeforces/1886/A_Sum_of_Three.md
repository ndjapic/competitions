# Problem: A_Sum_of_Three.pas

```pascal
program A_Sum_of_Three;
var
    ntc, tci: int16;
    n, x, y, z: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        if (n < 7) or (n = 9) then
            writeln('NO')
        else begin

            x := 1;

            if n mod 3 = 0 then
                y := 4
            else
                y := 2;
            z := n-x-y;

            writeln('YES');
            writeln(x, ' ', y, ' ', z);

        end;

    end;
end.

```
