# Задатак: C_How_Does_the_Rook_Move.pas

```pascal
program D_A_BIT_of_an_Inequality;
const
    maxn = 100 * 1000;
    maxe = 29;
var
    ntc, tci: int16;
    n, i, l, r: int32;
    e: int8;
    ans: int64;
    a: array [1 .. maxn] of int32;
    x: array [0 .. maxn] of int32;
    c: array [0 .. maxn] of array [0 .. maxe] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for e := 0 to maxe do c[0][e] := 0;
        x[0] := 0;

        for i := 1 to n do begin

            read(a[i]);
            x[i] := x[i-1] xor a[i];

            e := maxe;
            while a[i] shr e = 0 do dec(e);

            c[i] := c[i-1];
            inc(c[i][e]);

        end;
        readln;

        ans := 0;
        for r := 1 to n do
            for l := 1 to r do
                for e := 0 to maxe do
                    if not odd((x[r] xor x[l-1]) shr e) then
                        inc(ans, c[r][e] - c[l-1][e]);

        writeln(ans);

    end;
end.

```
