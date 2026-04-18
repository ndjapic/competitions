# Задатак: D_Permutation_Game.pas

```pascal
program D_Permutation_Game;
{$mode delphi}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, k, pb, ps: int32;
    b, s: int64;
    p, a, dsu: array [1 .. nn] of int32;

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union(u, v: int32);
begin
    u := find(u);
    v := find(v);
    if u = v then
    else if a[u] > a[v] then
        dsu[v] := u
    else
        dsu[u] := v;
end;

function score(i, root, k: int32): int64;
begin
    result := int64(k) * a[i];
    if (k > 0) and (a[i] < a[root]) then
        result := max(result, score(p[i], root, k-1) + a[i]);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k, pb, ps);
        for i := 1 to n do read(p[i]); readln;
        for i := 1 to n do read(a[i]); readln;

        for i := 1 to n do dsu[i] := i;
        for i := 1 to n do union(i, p[i]);

        b := score(pb, find(pb), k);
        s := score(ps, find(ps), k);

        if b > s then
            writeln('Bodya')
        else if s > b then
            writeln('Sasha')
        else
            writeln('Draw');

    end;
end.

```
