# Задатак: B_Deja_Vu.pas

```pascal
program B_Deja_Vu;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, q, i, j, m, t: int32;
    mn: int8;
    a: array [1 .. maxn] of int32;
    x: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, q);

        for j := 1 to n do read(a[j]); readln;

        t := 0;
        mn := 31;

        for i := 1 to q do begin

            read(x[i]);
            if x[i] < mn then begin

                mn := x[i];
                t := int32(1) shl (mn - 1);
                m := 2*t-1;
                for j := 1 to n do
                    if a[j] and m = 0 then inc(a[j], t);

            end;

        end;
        readln;

        for j := 1 to n-1 do write(a[j], ' '); writeln(a[n]);

    end;
end.

```
