# Problem: F_Circle_Perimeter.pas

```pascal
program F_Circle_Perimeter;
{$mode delphi}
var
    ntc, tci: int16;
    r, x, y, ans: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(r);

        ans := -4;
        x := 0;
        y := r+1;
        while x <= r do begin
            while (y-1 >= 0) and (sqr(x) + sqr(y-1) >= sqr(r)) do dec(y);
            while sqr(x) + sqr(y) < sqr(r+1) do begin
                inc(ans, 4);
                inc(y);
            end;
            inc(x);
        end;

        writeln(ans);

    end;
end.

```
