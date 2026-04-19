# Problem: C_Humidifier_3.pas

```pascal
program C_Humidifier_3;
{$mode delphi}
uses
    math;
const
    hh = 1000;
    rr = hh * hh;
var
    h, w, i, j: int16;
    d0, d, hw, l, r, humidified: int32;
    s: array [1 .. hh] of string;
    dist: array [1 .. hh, 1 .. hh] of int32;
    bfs: array [1 .. rr] of record
        i, j: int16;
    end;

procedure enqueue(d: int32; i, j: int16);
begin
    if (s[i][j] = '.') and (dist[i, j] > d) then begin
        dist[i, j] := d;
        inc(r);
        bfs[r].i := i;
        bfs[r].j := j;
    end;
end;

begin
    readln(h, w, d0);
    hw := h*w;

    r := 0;
    for i := 1 to h do begin
        readln(s[i]);
        for j := 1 to w do
            if s[i][j] = 'H' then begin
                inc(r);
                bfs[r].i := i;
                bfs[r].j := j;
                dist[i, j] := 0;
            end else
                dist[i, j] := high(int32);
    end;

    l := 1;
    while l <= r do begin
        i := bfs[l].i;
        j := bfs[l].j;
        d := dist[i, j] + 1;
        inc(l);

        if i > 1 then enqueue(d, i-1, j);
        if j > 1 then enqueue(d, i, j-1);
        if i < h then enqueue(d, i+1, j);
        if j < w then enqueue(d, i, j+1);
    end;

    humidified := 0;
    for i := 1 to h do
        for j := 1 to w do
            if dist[i, j] <= d0 then inc(humidified);

    writeln(humidified);
end.

```
