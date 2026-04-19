# Problem: C_Left_and_Right_Houses.pas

```pascal
program C_Left_and_Right_Houses;
{$H+}
const
    sz = 300 * 1000;
var
    ntc, tci: int16;
    n, i, best: int32;
    a: string;
    l, r: array [0 .. sz] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(a);

        l[0] := 0;
        r[0] := 0;

        for i := 1 to n do begin
            l[i] := l[i-1];
            r[i] := r[i-1];
            if a[i] = '0' then
                inc(l[i])
            else
                inc(r[i]);
        end;

        best := -1;
        for i := 0 to n do
            if (l[i] >= r[i]) and (r[n] - r[i] >= l[n] - l[i]) then begin
                if (best = -1) or (
                    abs(n - 2 * i) < abs(n - 2 * best)
                ) then best := i;
            end;

        writeln(best);

    end;
end.

```
