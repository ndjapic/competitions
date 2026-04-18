# Задатак: D_Home_Garden.pas

```pascal
program D_Home_Garden;
uses
    math;
const
    nn = 200 * 1000;
var
    q, i, tp, t, h, l, r, l0: int32;
    time: int64;
    a: array [1 .. nn] of int64;

begin
    readln(q);
    time := 0;
    l := 1;
    r := 0;

    for i := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                inc(r);
                a[r] := time;
            end;

            2: begin
                read(t);
                inc(time, t);
            end;

            3: begin
                read(h);
                l0 := l;
                while (l <= r) and (time - a[l] >= h) do inc(l);
                writeln(l - l0);
            end;

        end;
        readln;
    end;
end.

```
