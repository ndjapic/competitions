# Задатак: D_Square_Pair.pas

```pascal
program D_Square_Pair;
const
    maxn = 200 * 1000;
var
    n, i, ai, d, dd: int32;
    ans: int64;
    c: array [0 .. maxn] of int32;

begin
    for ai := 0 to maxn do c[ai] := 0;

    readln(n);

    for i := 1 to n do begin

        read(ai);
        if ai > 1 then begin

            d := 2;
            dd := d*d;
            while dd <= ai do begin
                while ai mod dd = 0 do ai := ai div dd;
                inc(d);
                dd := d*d;
            end;

        end;
        inc(c[ai]);

    end;
    readln;

    ans := 0;
    for ai := 0 to maxn do inc(ans, int64(c[ai] - 1) * c[ai]);
    ans := ans div 2 + int64(n-c[0]) * c[0];

    writeln(ans);
end.

```
