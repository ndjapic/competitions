# Задатак: A_Coin_Transformation.pas

```pascal
program A_Coin_Transformation;
var
    ntc, tci: int16;
    n, ans: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        ans := 1;
        while n > 3 do begin
            n := n div 4;
            inc(ans, ans);
        end;

        writeln(ans);

    end;
end.

```
