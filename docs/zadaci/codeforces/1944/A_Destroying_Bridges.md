# Задатак: A_Destroying_Bridges.pas

```pascal
program A_Destroying_Bridges;
var
    ntc, tci: int16;
    n, k: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n, k);
        if k >= n-1 then
            writeln(1)
        else
            writeln(n);
    end;
end.

```
