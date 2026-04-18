# Задатак: B_Vertical_Reading.pas

```pascal
program B_Vertical_Reading;
{$mode objfpc}{$H+}{$J-}
var
    s: string;
    n, m, w, c, i, j: int16;
    found: boolean;

begin
    readln(s);

    m := length(s);
    n := 1;
    while s[n] <> ' ' do inc(n);
    dec(n);

    w := 1;
    found := false;
    while (w <= n) and not found do begin

        c := 1;
        while (c <= w) and not found do begin

            i := c;
            j := n+2;
            while (i <= n) and (j <= m) and (s[i] = s[j]) do begin
                inc(i, w);
                inc(j);
            end;
            found := (i > n) and (j > m);

            inc(c);
        end;

        inc(w);
    end;

    if found then
        writeln('Yes')
    else
        writeln('No');
end.

```
