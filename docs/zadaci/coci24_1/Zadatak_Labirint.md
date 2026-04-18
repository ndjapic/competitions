# Задатак: Zadatak_Labirint.pas

```pascal
program Zadatak_Labirint;
uses
    math;
var
    n, m, i, j, q, k, a, b, c, d, mask: int8;
    hor, ver: array [1 .. 100, 1 .. 100] of char;
    ab, cd: array [1 .. 100] of int16;
    ans: array [1 .. 100] of int8;
    num: array ['A' .. 'Z'] of int8;
    dsu, sz: array [1 .. 10000] of int16;

function ij(i, j: int8): int16;
begin
    ij := int16(i-1) * m + j;
end;

function find(v: int16): int16;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union1(u, v: int16);
begin
    dsu[v] := u;
    inc(sz[u], sz[v]);
end;

procedure union2(u, v: int16);
begin
    u := find(u);
    v := find(v);
    if u = v then
        (* do nothing *)
    else if sz[u] > sz[v] then
        union1(u, v)
    else
        union1(v, u);
end;

procedure connect(mask: int8);
var
    i, j, k, e, noc: int8;
    v: int16;
begin

    for v := 1 to n*m do begin
        dsu[v] := v;
        sz[v] := 1;
    end;

    for i := 1 to n do
        for j := 1 to m-1 do begin
            e := num[hor[i, j]];
            if odd(mask shr e) then union2(ij(i, j), ij(i, j+1));
        end;

    for i := 1 to n-1 do
        for j := 1 to m do begin
            e := num[ver[i, j]];
            if odd(mask shr e) then union2(ij(i, j), ij(i+1, j));
        end;

    noc := 0;
    for e := 0 to 3 do
        if odd(mask shr e) then inc(noc);

    for k := 1 to q do
        if find(ab[k]) = find(cd[k]) then ans[k] := min(ans[k], noc);

end;

begin

    readln(n, m);

    for i := 1 to n do begin
        for j := 1 to m-1 do read(hor[i, j]);
        readln;
    end;

    for i := 1 to n-1 do begin
        for j := 1 to m do read(ver[i, j]);
        readln;
    end;

    readln(q);
    for k := 1 to q do begin
        readln(a, b, c, d);
        ab[k] := ij(a, b);
        cd[k] := ij(c, d);
        ans[k] := 4;
    end;

    num['P'] := 0;
    num['C'] := 1;
    num['Z'] := 2;
    num['N'] := 3;

    for mask := 1 to 14 do connect(mask);

    for k := 1 to q do writeln(ans[k]);

end.

```
