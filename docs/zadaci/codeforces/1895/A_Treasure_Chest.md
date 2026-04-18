# Задатак: A_Treasure_Chest.pas

```pascal
program A_Treasure_Chest;
uses
    math;
var
    ntc, tci, x, y, k, ans: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x, y, k);

        ans := x;
        if y > x then begin

            x := min(x+k, y);
            ans := y + (y-x);

        end;

        writeln(ans);

    end;
end.

```
