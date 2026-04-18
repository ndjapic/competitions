# Задатак: D_Come_a_Little_Closer.pas

```pascal
program D_Come_a_Little_Closer;
uses
    math;
const
    nn = 200 * 1000;
    inf = 1000 * 1000 * 1000;
    inf2 = int64(inf) * inf;
type
    tarr = array of int32;
var
    ntc, tci: int16;
    n, i: int32;
    ans: int64;
    x, y: tarr;
    monsters, cp: array [0 .. nn] of record
        x, y: int32;
    end;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (
                (monsters[il].y < monsters[ir].y) or
                (monsters[il].y = monsters[ir].y) and
                (monsters[il].x < monsters[ir].x)
            ) then begin
                cp[i] := monsters[il];
                inc(il);
            end else begin
                cp[i] := monsters[ir];
                inc(ir);
            end;

        for i := l to r-1 do monsters[i] := cp[i];

    end;
end;

procedure solve(x, y: tarr);
var
    i, l, r, dx, dy: int32;
    area: int64;
begin
    for i := 0 to n-1 do begin
        monsters[i].x := x[i];
        monsters[i].y := y[i];
    end;
    msort(0, n);

    l := inf;
    r := 1;
    for i := 1 to n-1 do begin
        l := min(l, monsters[i].x);
        r := max(r, monsters[i].x);
    end;

    dx := r-l+1;
    dy := monsters[n-1].y - monsters[1].y + 1;
    area := int64(dx) * dy;
    if area < n then inc(area, min(dx, dy));
    ans := min(ans, area);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        setlength(x, n);
        setlength(y, n);
        for i := 0 to n-1 do readln(x[i], y[i]);

        if n = 1 then
            ans := 1
        else begin

            ans := inf2;

            solve(x, y);
            solve(y, x);
            for i := 0 to n-1 do x[i] := inf+1 - x[i];
            solve(x, y);
            solve(y, x);
            for i := 0 to n-1 do y[i] := inf+1 - y[i];
            solve(x, y);
            solve(y, x);
            for i := 0 to n-1 do x[i] := inf+1 - x[i];
            solve(x, y);
            solve(y, x);

        end;

        writeln(ans);

    end;
end.

```
