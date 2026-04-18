# Задатак: E_Boneca_Ambalabu.pas

```pascal
program E_Boneca_Ambalabu;
uses
    math;
const
    nn = 200 * 1000;
    ee = 29;
var
    ntc, tci: int16;
    n, i: int32;
    e: int8;
    s, ans: int64;
    a: array [1 .. nn] of int32;
    c: array [0 .. ee] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for e := 0 to ee do c[e] := 0;

        for i := 1 to n do begin
            read(a[i]);
            for e := 0 to ee do
                if odd(a[i] shr e) then
                    inc(c[e]);
        end;
        readln;

        ans := 0;
        for i := 1 to n do begin
            s := 0;
            for e := 0 to ee do
                if odd(a[i] shr e) then
                    inc(s, int64(n - c[e]) shl e)
                else
                    inc(s, int64(c[e]) shl e);
            ans := max(ans, s);
        end;

        writeln(ans);

    end;
end.

```
