# Problem: A_Online_Shopping.pas

```pascal
program A_Online_Shopping;
var
    n, i, k, p, q, s, x: int32;

begin
    readln(n, s, k);

    x := 0;
    for i := 1 to n do begin
        readln(p, q);
        inc(x, p*q);
    end;

    if x < s then inc(x, k);
    writeln(x);
end.

```
