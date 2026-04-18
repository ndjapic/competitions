# Задатак: C_Assembly_via_Remainders.pas

```pascal
program C_Assembly_via_Remainders;
const
    nn = 500;
    inf = nn + 1;
var
    ntc, tci: int16;
    n, i, j: int16;
    d: int32;
    a, x, q: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 2 to n do read(x[i]); readln;

        a[1] := inf;
        for i := 2 to n do begin
            q[i] := 0;
            a[i] := a[i-1] * q[i] + x[i];
            for j := i-1 downto 2 do
                if x[j+1] >= a[j] then begin
                    d := x[j+1] - a[j] + 1;
                    inc(q[j], (d-1) div a[j-1] + 1);
                    a[j] := a[j-1] * q[j] + x[j];
                end;
        end;

        for i := 1 to n-1 do write(a[i], ' '); writeln(a[n]);

    end;
end.

```
