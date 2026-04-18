# Задатак: C_Find_B.pas

```pascal
program C_Find_B;
const
    maxn = 300 * 1000;
var
    ntc, tci: int16;
    n, q, i, x, k, l, r: int32;
    c: array [0 .. maxn] of int32;
    s: array [0 .. maxn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, q);

        c[0] := 0;
        s[0] := 0;

        for i := 1 to n do begin
            read(x);
            c[i] := c[i-1];
            if x = 1 then inc(c[i]);
            s[i] := s[i-1] + x-1;
        end;
        readln;

        for k := 1 to q do begin
            readln(l, r);
            if (l < r) and (c[r] - c[l-1] <= s[r] - s[l-1]) then
                writeln('YES')
            else
                writeln('NO');
        end;

    end;

end.

```
