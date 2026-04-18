program E_Sightseeing_Tour;
{$mode objfpc}
uses
    math;
const
    nn = 400;
    mm = 200 * 1000;
    inf = int64(1000) * 1000 * 1000 * 1000 * 1000 * 1000;
var
    n, m, q, i, j, k, ui, vi, wi, ti: int32;
    loop: boolean;
    s: int64;
    time: array [1 .. nn, 1 .. nn] of int64;
    u, v, t: array [1 .. mm] of int32;
    b: array [1 .. 5] of int32;

function dfs(i, wi: int32): int64;
var
    j, bi: int32;
begin
    if i = 0 then
        result := time[1, wi]
    else begin
        bi := b[i];
        result := inf;
        for j := 1 to i do begin

            b[i] := b[j];
            b[j] := bi;
            result := min(result,
                dfs(i-1, u[b[i]]) + t[b[i]] + time[v[b[i]], wi]);
            result := min(result,
                dfs(i-1, v[b[i]]) + t[b[i]] + time[u[b[i]], wi]);
            b[j] := b[i];
            b[i] := bi;

        end;
    end;
end;

begin
    readln(n, m);

    for ui := 1 to n do begin
        for vi := 1 to n do time[ui, vi] := inf;
        time[ui, ui] := 0;
    end;

    for i := 1 to m do begin
        readln(ui, vi, ti);
        time[ui, vi] := min(time[ui, vi], ti);
        time[vi, ui] := time[ui, vi];
        u[i] := ui;
        v[i] := vi;
        t[i] := ti;
    end;

    loop := true;
    while loop do begin
        loop := false;
        for ui := 1 to n do
            for vi := 1 to n do
                for wi := 1 to n do begin
                    s := time[ui, vi] + time[vi, wi];
                    if time[ui, wi] > s then begin
                        time[ui, wi] := s;
                        loop := true;
                    end;
                end;
    end;

    readln(q);
    for j := 1 to q do begin
        readln(k);
        for i := 1 to k do read(b[i]);
        readln;
        writeln(dfs(k, n));
    end;
end.
