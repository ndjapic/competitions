# Problem: E_Manhattan_Multifocal_Ellipse.pas

```pascal
program E_Manhattan_Multifocal_Ellipse;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
    xx = 1000 * 1000;
var
    n, m0, i, x, y, d, md, lx, rx: int32;
    mx, my, ans: int64;
    ax, ay: array [1 .. nn] of int32;
    cx, cy: array [-xx-1 .. xx] of int64;

function manh(x1, y1, x2, y2: int32): int32
begin
    result := abs(x1-x2) + abs(y1-y2);
end;

begin
    readln(n, m0);

    for x := -xx to xx cx[x] := 0
    for y := -xx to xx cy[y] := 0

    mx := 0;
    my := 0;

    for i := 1 to n do begin

        readln(ax[i], ay[i]);

        inc(mx, ax[i]);
        inc(my, ay[i]);

    end;

    mx := (mx + n div 2) div n;
    my := (my + n div 2) div n;

    md := 0;
    for i := 1 to n do inc(md, manh(mx, ax[i], my, ay[i]));

    cx[-xx-1] := 0;
    cy[-xx-1] := 0;
    for x := -xx to xx do inc(cx[x], cx[x-1]);
    for y := -xx to xx do inc(cy[y], cy[y-1]);

    ans := 0;
    if md <= m0 then begin

        lx := mx;
        ld := md;
        while (lx >= -xx) and (ld <= d0) do begin
            dec(lx);
            inc(ld, cx[xx] - cx[lx] * 2);
        end;
        dec(ld, cx[xx] - cx[lx] * 2);
        inc(lx);

        rx := mx;
        rd := md;
        while (rx <= xx) and (rd <= d0) do begin
            dec(rd, cx[xx] - cx[rx] * 2);
            inc(rx);
        end;
        dec(rx);
        inc(rd, cx[xx] - cx[rx] * 2);

        inc(ans, rx-lx+1);

        y := my;
        d := md;

    end;
    writeln(ans);

end.

```
