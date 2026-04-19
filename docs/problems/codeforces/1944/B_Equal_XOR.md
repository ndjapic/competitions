# Problem: B_Equal_XOR.pas

```pascal
program B_Equal_XOR;
const
    maxn = 50 * 1000;
var
    ntc, tci: int16;
    n, k, i, j, x, y: int32;
    a: array [1 .. maxn * 2] of int32;
    l, r: array [1 .. maxn] of int32;
    c: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to 2*n do read(a[i]);

        for x := 1 to n do c[x] := 0;

        for i := 1 to n do inc(c[a[i]]);

        j := 1;
        x := 1;
        y := 1;

        while (j <= 2*k) and (x <= n) and (y <= n) do begin
            while (x <= n) and (c[x] < 2) do inc(x);
            while (y <= n) and (c[y] > 0) do inc(y);
            if (x <= n) and (y <= n) then begin
                l[j] := x;
                r[j] := y;
                inc(j);
                l[j] := x;
                r[j] := y;
                inc(j);
                inc(x);
                inc(y);
            end;
        end;

        x := 1;
        while j <= 2*k do begin
            while (x <= n) and (c[x] <> 1) do inc(x);
            l[j] := x;
            r[j] := x;
            inc(j);
            inc(x);
        end;

        for i := 1 to 2*k-1 do write(l[i], ' '); writeln(l[2*k]);
        for i := 1 to 2*k-1 do write(r[i], ' '); writeln(r[2*k]);

    end;
end.

```
