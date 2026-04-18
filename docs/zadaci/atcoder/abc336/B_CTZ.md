# Задатак: B_CTZ.pas

```pascal
program B_CTZ;
var
    n: int32;
    ctz: int8;

begin
    readln(n);
    ctz := 0;
    while not odd(n) do begin
        n := n div 2;
        inc(ctz);
    end;
    writeln(ctz);
end.

```
