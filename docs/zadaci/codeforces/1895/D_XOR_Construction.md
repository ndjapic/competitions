# Задатак: D_XOR_Construction.pas

```pascal
program D_XOR_Construction;
uses
    math;
const
    maxn = 200 * 1000;
var
    {ntc,} tci, n, i: int32;
    e: int8;
    b: array [1 .. maxn] of int32;
    c: array [0 .. 29] of int32;

begin
    {readln(ntc);}
    for tci := 1 to 1 do begin

        readln(n);

        b[1] := 0;
        for e := 0 to 29 do c[e] := 0;

        for i := 2 to n do begin
            read(b[i]);
            b[i] := b[i-1] xor b[i];
            for e := 0 to 29 do
                if odd(b[i] shr e) then
                    inc(c[e])
                else
                    dec(c[e]);
        end;
        readln;

        for e := 0 to 29 do
            if c[e] > 0 then inc(b[1], int32(1) shl e);

        write(b[1]);
        for i := 2 to n do write(' ', b[i] xor b[1]);
        writeln;

    end;
end.

```
