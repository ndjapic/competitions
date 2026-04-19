# Problem: C_MEX_Game_1.pas

```pascal
program C_MEX_Game_1;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, x: int32;
    c: array [0 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for x := 0 to n do c[x] := 0;

        for i := 1 to n do begin
            read(x);
            inc(c[x]);
        end;
        readln;

        x := 0;
        while c[x] > 1 do inc(x);

        if c[x] = 1 then begin
            inc(x);
            while c[x] > 1 do inc(x);
        end;

        writeln(x);

    end;
end.

```
