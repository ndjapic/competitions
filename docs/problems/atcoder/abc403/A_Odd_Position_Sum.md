# Problem: A_Odd_Position_Sum.pas

```pascal
program A_Odd_Position_Sum;
var
    n, i, x, s: int32;

begin
    readln(n);

    s := 0;
    for i := 1 to n do begin
        read(x);
        if odd(i) then inc(s, x);
    end;
    readln;

    writeln(s);
end.

```
