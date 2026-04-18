# Задатак: C_Maximum_Subarray_Sum.pas

```pascal
program C_Maximum_Subarray_Sum;
{$MODE DELPHI}
uses
    math;
const
    nn = 200 * 1000 + 10;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, i, m, j: int32;
    k, x, mx, d: int64;
    found: boolean;
    s: string;
    a: array [0 .. nn] of int64;
    lim: array [0 .. nn] of int32;

begin
    a[0] := 0;
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, k);
		readln(s);

        lim[0] := 0;
        m := 0;
        for i := 1 to n do
            if s[i] = '0' then begin
                inc(m);
                lim[m] := i;
            end;
        inc(m);
        lim[m] := n+1;

        for i := 1 to n do read(a[i]); readln;

        x := 0;
        for j := 1 to m do
            if lim[j] - lim[j-1] > 0 then begin

                mx := -inf;
                inc(x, a[lim[j-1]]);
                if j > 1 then mx := max(mx, x);
                x := max(x, 0);
                found := false;
                d := 0;

                for i := lim[j-1]+1 to lim[j]-1 do begin
                    inc(x, a[i]);
                    mx := max(mx, x);
                    if x < 0 then found := true;
                    if (j > 1) and not found then
                        d := max(d, mx - k);
                    x := max(x, 0);
                end;

                if j < m then a[lim[j]] := max(k - max(mx, 0), 0);
                if j > 1 then dec(a[lim[j-1]], d)

            end;

        mx := -inf;
        x := 0;
        for i := 1 to n do begin
            inc(x, a[i]);
            mx := max(mx, x);
            x := max(x, 0);
        end;

        if mx = k then begin
            writeln('Yes');
            for i := 1 to n-1 do write(a[i], ' ');
            writeln(a[n]);
        end else
            writeln('No');

    end;
end.

```
