# Задатак: C_Even_Digits.pas

```pascal
program C_Even_Digits;
var
    n: int64;
    nd, i: int8;
    d: array [1 .. 100] of int8;

begin
    readln(n);
    dec(n);
    nd := 0;

    while n > 0 do begin
        inc(nd);
        d[nd] := n mod 5 * 2;
        n := n div 5;
    end;

    for i := nd downto 1 do write(d[i]);
    if nd = 0 then write(0);
    writeln;
end.

```
