# Задатак: D_Snaky_Walk.pas

```pascal
program D_Snaky_Walk;
{$mode delphi}
uses
    math;
const
    nn = 1000;
var
    h, w, i, j, si, sj, gi, gj: int16;
    d, inf, l, r: int32;
    dir: int8;
    s: array [1 .. nn] of string;
    dist: array [1 .. nn, 1 .. nn, 0 .. 1] of int32;
    bfs: array of record
        d: int32;
        i, j: int16;
        dir: int8;
    end;

procedure enque_checked(d: int32; i, j: int16; dir: int8);
begin
    if r = length(bfs) then setlength(bfs, 2*r);
    dist[i, j, dir] := d;
    bfs[r].d := d;
    bfs[r].i := i;
    bfs[r].j := j;
    bfs[r].dir := dir;
    inc(r);
end;

procedure enque(d: int32; i, j: int16; dir: int8);
begin
    if (s[i][j] <> '#') and (dist[i, j, dir] > d) then
        enque_checked(d, i, j, dir);
end;

begin
    readln(h, w);
    inf := h*w;

    for i := 1 to h do begin
        readln(s[i]);

        for j := 1 to w do
            if s[i][j] <> '#' then begin
                dist[i, j, 0] := inf;
                dist[i, j, 1] := inf;

                if s[i][j] = 'S' then begin
                    si := i;
                    sj := j;
                end else if s[i][j] = 'G' then begin
                    gi := i;
                    gj := j;
                end;
            end;
    end;

    l := 0;
    r := 0;
    setlength(bfs, 1);
    enque_checked(0, si, sj, 0);
    enque_checked(0, si, sj, 1);

    while l < r do begin
        d := bfs[l].d + 1;
        i := bfs[l].i;
        j := bfs[l].j;
        dir := 1 - bfs[l].dir;
        inc(l);

        if dir = 1 then begin
            if 1 < i then enque(d, i-1, j, dir);
            if i < h then enque(d, i+1, j, dir);
        end else begin
            if 1 < j then enque(d, i, j-1, dir);
            if j < w then enque(d, i, j+1, dir);
        end;
    end;

    {for i := 1 to h do begin
        for j := 1 to w do
            if s[i][j] = '#' then
                write(-1:4)
            else
                write(dist[i][j]:4);
        writeln;
    end;}

    d := min(dist[gi, gj, 0], dist[gi, gj, 1]);
    if d = inf then d := -1;
    writeln(d);
end.

```
