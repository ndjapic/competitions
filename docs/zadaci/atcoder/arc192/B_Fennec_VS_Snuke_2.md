# Задатак: B_Fennec_VS_Snuke_2.pas

```pascal
program B_Fennec_VS_Snuke_2;
var
    n, i, x, c1: int32;
    win: boolean;

begin
    readln(n);

    c1 := 0;
    for i := 1 to n do begin
        read(x);
        inc(c1, x mod 2);
    end;
    readln;

    if n < 3 then
        win := n = 1
    else if n = 3 then
        win := c1 > 0
    else
        win := odd(c1);

    if win then
        writeln('Fennec')
    else
        writeln('Snuke');
end.

```
