# Задатак: D_Cuboid_Sum_Query.pas

```pascal
program D_Cuboid_Sum_Query;
const
    nn = 100;
var
    n, x, y, z, lx, rx, ly, ry, lz, rz: int8;
    q, i, s: int32;
    a: array [0 .. nn, 0 .. nn, 0 .. nn] of int32;

begin
    readln(n);

    for x := 0 to n do
        for y := 0 to n do begin
            for z := 0 to n do
                if (x = 0) or (y = 0) or (z = 0) then
                    a[x, y, z] := 0
                else begin
                    read(a[x, y, z]);
                    inc(a[x, y, z], a[x, y, z-1]);
                    inc(a[x, y, z], a[x, y-1, z]);
                    inc(a[x, y, z], a[x-1, y, z]);
                    dec(a[x, y, z], a[x-1, y-1, z]);
                    dec(a[x, y, z], a[x-1, y, z-1]);
                    dec(a[x, y, z], a[x, y-1, z-1]);
                    inc(a[x, y, z], a[x-1, y-1, z-1]);
                end;
            if (x > 0) and (y > 0) then readln;
        end;

    readln(q);
    for i := 1 to q do begin

        readln(lx, rx, ly, ry, lz, rz);
        s := a[rx, ry, rz];

        dec(s, a[rx, ry, lz-1]);
        dec(s, a[rx, ly-1, rz]);
        dec(s, a[lx-1, ry, rz]);
        inc(s, a[lx-1, ly-1, rz]);
        inc(s, a[lx-1, ry, lz-1]);
        inc(s, a[rx, ly-1, lz-1]);
        dec(s, a[lx-1, ly-1, lz-1]);

        writeln(s);

    end;
end.

```
