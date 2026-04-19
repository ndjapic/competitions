# Problem: A_Rhythm_Game.pas

```pascal
program A_Rhythm_Game;
uses
    math;
const
    nn = 5000;
var
    ntc, tci: int32;
    n, i, j, rank: int16;
    d, time: int64;
    winnable: boolean;
    t, x: array [0 .. nn] of int64;
    p, cp: array [0 .. nn] of int32;

procedure lsort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        lsort(l, m);
        lsort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (
                t[p[il]] - x[p[il]] <= t[p[ir]] - x[p[ir]]
            ) then begin
                cp[i] := p[il];
                inc(il);
            end else begin
                cp[i] := p[ir];
                inc(ir);
            end;

        for i := l to r-1 do p[i] := cp[i];

    end;
end;

procedure rsort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        rsort(l, m);
        rsort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (
                t[p[il]] + x[p[il]] <= t[p[ir]] + x[p[ir]]
            ) then begin
                cp[i] := p[il];
                inc(il);
            end else begin
                cp[i] := p[ir];
                inc(ir);
            end;

        for i := l to r-1 do p[i] := cp[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);
        readln(d);

        for i := 0 to n-1 do readln(t[i], x[i]);

        winnable := false;
        j := 0;
        while not winnable and (j <= n) do begin

            for i := 0 to n-1 do p[i] := i;
            lsort(0, j);
            rsort(j, n);

            winnable := true;
            rank := 0;
            time := 0;
            while winnable and (rank < n) do begin
                i := p[rank];
                inc(time, x[i]);
                time := max(time, t[i]);
                winnable := time <= t[i] + d;
                inc(time, x[i]);
                inc(rank);
            end;

            inc(j);
        end;

        if winnable then
            writeln('Yes')
        else
            writeln('No');
    end;
end.

```
