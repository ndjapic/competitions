# Задатак: A_Sort_Left_and_Right.pas

```pascal
program A_Sort_Left_and_Right;
uses
    math;
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int32;
    n, i: int32;
    p, mn, mx: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(p[i]); readln;

        i := 1;
        while (i < n) and (p[i] < p[i+1]) do inc(i);

        if i = n then
            writeln(0)
        else begin

            mx[0] := 0;
            mn[n+1] := n+1;
            for i := 1 to n do mx[i] := max(mx[i-1], p[i]);
            for i := n downto 1 do mn[i] := min(mn[i+1], p[i]);

            i := 1;
            while (i <= n) and not (
                (mx[i-1] < p[i]) and (p[i] = i)
            ) do inc(i);

            if i <= n then
                writeln(1)
            else begin

                i := 1;
                while (i < n) and not (mx[i] <= i) do inc(i);

                if (i < n) or (p[1] < n) or (p[n] > 1) then
                    writeln(2)
                else
                    writeln(3);

            end;

        end;

    end;
end.

```
