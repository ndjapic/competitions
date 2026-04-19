# Problem: D_Goin_to_the_Zoo.pas

```pascal
program D_Goin_to_the_Zoo;
uses
    math;
const
    nn = 10;
    mm = 100;
    inf = int64(1) shl 60;
var
    n, m, i, k, zoo, ani, vis: int8;
    p3, p, x: int32;
    fee, minfee: int64;
    a: array [1 .. mm, 1 .. nn] of boolean;
    c: array [1 .. nn] of int64;
    saw: array [1 .. mm] of int16;

begin
    readln(n, m);
    p3 := 1;

    for zoo := 1 to n do begin
        read(c[zoo]);
        for ani := 1 to m do a[ani, zoo] := false;
        p3 := p3 * 3;
    end;
    readln;

    for ani := 1 to m do begin
        read(k);
        for i := 1 to k do begin
            read(zoo);
            a[ani, zoo] := true;
        end;
        readln;
    end;

    minfee := inf;
    for p := 0 to p3-1 do begin
        fee := 0;

        for ani := 1 to m do saw[ani] := 0;

        x := p;
        for zoo := 1 to n do begin
            vis := x mod 3;
            x := x div 3;
            inc(fee, c[zoo] * vis);

            for ani := 1 to m do
                if a[ani, zoo] then
                    inc(saw[ani], vis);
        end;

        ani := 1;
        while (ani <= m) and (saw[ani] >= 2) do inc(ani);

        if ani > m then minfee := min(minfee, fee);
    end;

    writeln(minfee);
end.

```
