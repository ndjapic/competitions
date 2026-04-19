# Problem: C_Bessie_s_Birthday_Cake.pas

```pascal
program C_Bessie_s_Birthday_Cake;
uses
    math;
const
    maxx = 200 * 1000;
type
    tarr = array [0 .. maxx] of int32;
var
    ntc, tci: int16;
    n, i, x, y, v: int32;
    triangles: int32;
    choosen, merge, poly: tarr;

procedure msort(var a: tarr; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(a, l, m);
        msort(a, m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (a[j] <= a[k]) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x, y);

        for i := 1 to x do read(choosen[i]); readln;
        msort(choosen, 1, x+1);
        choosen[0] := choosen[x] - n;

        for i := 1 to x do poly[i] := choosen[i] - choosen[i-1] + 1;

        msort(poly, 1, x+1);
        triangles := x-2;

        for i := 1 to x do begin
            v := (poly[i] - 3) div 2;
            if (poly[i] >= 3) and (y >= v) and odd(poly[i]) then begin
                inc(triangles, 2*v);
                dec(poly[i], 2*v);
                dec(y, v);
            end;
        end;

        for i := 1 to x do begin
            v := min(y, (poly[i] - 2) div 2);
            inc(triangles, 2*v);
            if poly[i] = 3 then inc(triangles);
            dec(y, v);
        end;

        writeln(triangles);

    end;
end.

```
