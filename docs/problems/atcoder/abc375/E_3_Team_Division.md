# Problem: E_3_Team_Division.pas

```pascal
program E_3_Team_Division;
{$mode delphi}
uses
    math;
const
    nn = 100;
    bb = 500;
var
    n, i, w, w1: int8;
    s, x, y, z, x1, y1, z1: int16;
    l, r: int32;
    a: array [1 .. nn] of int8;
    b: array [1 .. nn] of int16;
    d: array [0 .. bb, 0 .. bb, 0 .. bb] of int8;
    bfs: array of record
        x, y, z: int16;
        i: int8;
    end;

procedure enqueue(x, y, z: int16; i, w: int8);
begin
    if w < d[x, y, z] then begin
        d[x, y, z] := w;
        inc(r);
        if length(bfs) = r then setlength(bfs, 2*r);
        bfs[r].x := x;
        bfs[r].y := y;
        bfs[r].z := z;
        bfs[r].i := i;
    end;
end;

begin
    readln(n);

    s := 0;
    for i := 1 to n do begin
        readln(a[i], b[i]);
        inc(s, b[i]);
    end;

    if s mod 3 > 0 then
        w := -1
    else begin

        s := s div 3;
        for x := 0 to s do
            for y := 0 to s do
                for z := 0 to s do
                    d[x, y, z] := n+1;
        d[0, 0, 0] := 0;

        l := 0;
        r := 0;
        setlength(bfs, 1);
        bfs[0].x := 0;
        bfs[0].y := 0;
        bfs[0].z := 0;
        bfs[0].i := 0;

        while l <= r do begin

            x := bfs[l].x;
            y := bfs[l].y;
            z := bfs[l].z;
            i := bfs[l].i + 1;
            w := d[x, y, z];
            inc(l);

            if i <= n then begin

                x1 := x + b[i];
                y1 := y + b[i];
                z1 := z + b[i];
                w1 := w + 1;

                case a[i] of

                    1: begin
                        if x1 <= s then enqueue(x1, y, z, i, w);
                        if y1 <= s then enqueue(x, y1, z, i, w1);
                        if z1 <= s then enqueue(x, y, z1, i, w1);
                    end;

                    2: begin
                        if y1 <= s then enqueue(x, y1, z, i, w);
                        if z1 <= s then enqueue(x, y, z1, i, w1);
                        if x1 <= s then enqueue(x1, y, z, i, w1);
                    end;

                    3: begin
                        if z1 <= s then enqueue(x, y, z1, i, w);
                        if x1 <= s then enqueue(x1, y, z, i, w1);
                        if y1 <= s then enqueue(x, y1, z, i, w1);
                    end;

                end;

            end;

        end;

        w := d[s, s, s];
        if w > n then w := -1;

    end;

    writeln(w);
end.

```
