# Problem: D_Synchronized_Players.pas

```pascal
program D_Synchronized_Players;
uses
    math;
const
    maxn = 60;
    inf = 10000;
var
    n, i, j, k, i1, j1, i2, j2: int8;
    d1, ans: int16;
    l, r: int32;
    s: array [1 .. maxn, 1 .. maxn] of char;
    d: array [1 .. maxn, 1 .. maxn, 1 .. maxn, 1 .. maxn] of int16;
    bfs: array of record
        i1, j1, i2, j2: int8;
    end;

function regular(i, j: int8): boolean;
begin
    regular := (i >= 1) and (i <= n) and (j >= 1) and (j <= n) and (s[i, j] <> '#');
end;

procedure enqueue(i1, j1, i2, j2, i3, j3, i4, j4: int8; d1: int16);
begin
    if not regular(i3, j3) then begin
        i3 := i1;
        j3 := j1;
    end;

    if not regular(i4, j4) then begin
        i4 := i2;
        j4 := j2;
    end;

    if d[i3, j3, i4, j4] > d1 then begin
        d[i3, j3, i4, j4] := d1;
        if r = length(bfs) then setlength(bfs, 2*r);
        bfs[r].i1 := i3;
        bfs[r].j1 := j3;
        bfs[r].i2 := i4;
        bfs[r].j2 := j4;
        inc(r);
    end;
end;

begin
    readln(n);

    for i1 := 1 to n do
        for j1 := 1 to n do
            for i2 := 1 to n do
                for j2 := 1 to n do
                    d[i1, j1, i2, j2] := inf;

    k := 1;
    for i := 1 to n do begin
        for j := 1 to n do begin
            read(s[i, j]);
            if s[i, j] = 'P' then
                case k of
                    1: begin
                        i1 := i;
                        j1 := j;
                        inc(k);
                    end;
                    2: begin
                        i2 := i;
                        j2 := j;
                    end;
                end;
        end;
        readln;
    end;

    l := 0;
    r := 0;
    setlength(bfs, 1);
    enqueue(i1, j1, i2, j2, i1, j1, i2, j2, 0);

    while l < r do begin
        i1 := bfs[l].i1;
        j1 := bfs[l].j1;
        i2 := bfs[l].i2;
        j2 := bfs[l].j2;
        d1 := d[i1, j1, i2, j2] + 1;

        enqueue(i1, j1, i2, j2, i1-1, j1, i2-1, j2, d1);
        enqueue(i1, j1, i2, j2, i1+1, j1, i2+1, j2, d1);
        enqueue(i1, j1, i2, j2, i1, j1-1, i2, j2-1, d1);
        enqueue(i1, j1, i2, j2, i1, j1+1, i2, j2+1, d1);

        inc(l);
    end;

    ans := inf;
    for i := 1 to n do
        for j := 1 to n do
            ans := min(ans, d[i, j, i, j]);

    if ans = inf then ans := -1;
    writeln(ans);
end.

```
