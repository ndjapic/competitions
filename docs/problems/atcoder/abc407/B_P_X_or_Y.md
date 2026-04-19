# Problem: B_P_X_or_Y.pas

```pascal
program B_P_X_or_Y;
var
    x, y, i, j, c: int32;

begin
    readln(x, y);

    c := 0;
    for i := 1 to 6 do
        for j := 1 to 6 do
            if (i+j >= x) or (abs(i-j) >= y) then inc(c);

    writeln(c / 36:12:10);
end.

```
