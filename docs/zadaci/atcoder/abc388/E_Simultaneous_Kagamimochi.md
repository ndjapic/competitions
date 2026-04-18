# Задатак: E_Simultaneous_Kagamimochi.pas

```pascal
program E_Simultaneous_Kagamimochi;
const
    nn = 500 * 1000;
var
    n, i, k, l, r: int32;
    a: array [1 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n do read(a[i]);
    readln;

    k := 0;
    r := n;
    for l := n div 2 downto 1 do
        if 2 * a[l] <= a[r] then begin
            inc(k);
            dec(r);
        end;

    writeln(k);
end.

```
