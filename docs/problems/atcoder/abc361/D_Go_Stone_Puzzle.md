# Problem: D_Go_Stone_Puzzle.pas

```pascal
program D_Go_Stone_Puzzle;
{$mode delphi}{$inline on}
uses
    math;
const
    nn = 14;
var
    n, m, i, j, m1, m2, m3, d, s, t, p2, e, l, r: int32;
    sr, tr: string;
    dist: array [0 .. 16383, 0 .. nn] of int32;
    bfs: array of record
        m, i, d: int32;
    end;

procedure enqueque(m, i, d: int32);
begin
    if dist[m, i] > d then begin
        dist[m, i] := d;
        inc(r);
        if length(bfs) = r then setlength(bfs, 2*r);
        bfs[r].m := m;
        bfs[r].i := i;
        bfs[r].d := d;
    end;
end;

begin
    readln(n);
    readln(sr);
    readln(tr);

    s := 0;
    t := 0;
    p2 := 1;
    for e := 0 to n-1 do begin
        if sr[n-e] = 'B' then inc(s, p2);
        if tr[n-e] = 'B' then inc(t, p2);
        inc(p2, p2);
    end;

    for m := 0 to (1 shl n) - 1 do
        for i := 0 to n do
            dist[m, i] := high(int32);
    dist[s, 0] := 0;

    setlength(bfs, 1);
    bfs[0].m := s;
    bfs[0].i := 0;
    bfs[0].d := 0;

    l := 0;
    r := 0;
    while l <= r do begin
        m := bfs[l].m;
        i := bfs[l].i;
        d := bfs[l].d;

        for j := 1 to i-1 do begin
            m1 := (m shr i) shl i;
            m2 := (m shr (j+1)) shl (j+1);
            m3 := (m shr (j-1)) shl (j-1);
            enqueque(
                m-m3+m1 + ((m2-m1) shr 2) + ((m3-m2) shl (i-(j+1))),
                j-1, d+1);
        end;

        for j := i+1 to n-1 do begin
            m1 := (m shr (j+1)) shl (j+1);
            m2 := (m shr (j-1)) shl (j-1);
            m3 := (m shr i) shl i;
            enqueque(
                m-m3+m1 + ((m3-m2) shl 2) + ((m2-m1) shr ((j-1)-i)),
                j+1, d+1);
        end;

        inc(l);
    end;

    if dist[t, 0] < high(int32) then
        writeln(dist[t, 0])
    else
        writeln(-1);
end.

```
