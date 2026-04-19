# Problem: D_Laser_Marking.pas

```pascal
program D_Laser_Marking;
{$mode delphi}{$inline on}
var
    n, i: int8;
    s, t, mn: extended;
    a: array [0 .. 12] of record
        x, y: int32;
    end;
    p: array [1 .. 6] of int8;

function dist(i, j: int8): extended; inline;
begin
    result := 1.0;
    result := sqrt(result * sqr(a[i].x - a[j].x) + sqr(a[i].y - a[j].y));
end;

procedure dfs(i: int8; k2: int16; time: extended);
var
    j, k: int8;
    t0: extended;
begin
    if i <= n then
        for j := i to n do begin
            k := p[j];
            p[j] := p[i];
            t0 := dist(2*k-1, 2*k-0) / t;

            dfs(i+1, 2*k-0, time + dist(k2, 2*k-1) / s + t0);
            dfs(i+1, 2*k-1, time + dist(k2, 2*k-0) / s + t0);

            p[i] := p[j];
            p[j] := k;
        end
    else if (mn < 0) or (time < mn) then
        mn := time;
end;

begin
    readln(n, s, t);

    a[0].x := 0;
    a[0].y := 0;
    for i := 1 to n do begin
        readln(a[2*i-1].x, a[2*i-1].y, a[2*i].x, a[2*i].y);
        p[i] := i;
    end;

    mn := -1.0;
    dfs(1, 0, 0.0);
    writeln(mn:13:7);
end.

```
