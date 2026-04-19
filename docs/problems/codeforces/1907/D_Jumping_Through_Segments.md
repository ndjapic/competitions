# Problem: D_Jumping_Through_Segments.pas

```pascal
program D_Jumping_Through_Segments; {$H+}
uses
    math;
const
    maxn = 200 * 1000;
    maxk = 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, i, mn, mx, k, xl, xr: int32;
    l, r: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do readln(l[i], r[i]);

        mn := -1;
        mx := maxk;
        while mx - mn > 1 do begin

            k := (mn + mx) div 2;
            i := 1;
            xl := 0;
            xr := 0;

            while (i <= n) and (xl <= xr) do begin
                xl := max(l[i], xl - k);
                xr := min(r[i], xr + k);
                inc(i);
            end;

            if xl <= xr then
                mx := k
            else
                mn := k;

        end;

        writeln(mx);

    end;
end.

```
