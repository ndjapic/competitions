# Problem: D_Grid_and_Magnet.pas

```pascal
program D_Grid_and_Magnet;
{$H+}
uses
    math;
const
    szs = 1000;
    szu = szs * szs;
var
    h, w, i, j, v, mx: int32;
    s: array [1 .. szs] of string;
    dsu, size: array [1 .. szu] of int32;

function ij(i, j: int32): int32;
begin
    ij := (i-1)*w + j;
end;

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union2(u, v: int32);
begin
    dsu[v] := u;
    inc(size[u], size[v]);
end;

procedure union1(u, v: int32);
begin
    u := find(u);
    v := find(v);
    if u = v then
    else if size[u] > size[v] then
        union2(u, v)
    else
        union2(v, u);
end;

begin
    readln(h, w);

    for i := 1 to h do readln(s[i]);

    for i := 1 to h do
        for j := 1 to w do begin
            v := ij(i, j);
            dsu[v] := v;
            if s[i][j] = '#' then
                size[v] := 0
            else
                size[v] := 1;
        end;

    for i := 1 to h do
        for j := 1 to w do
            if s[i][j] = '#' then begin
                if (j > 1) and (s[i][j-1] = '.') then s[i][j-1] := '1';
                if (j < w) and (s[i][j+1] = '.') then s[i][j+1] := '1';
                if (i > 1) and (s[i-1][j] = '.') then s[i-1][j] := '1';
                if (i < h) and (s[i+1][j] = '.') then s[i+1][j] := '1';
            end;

    for i := 1 to h do
        for j := 1 to w-1 do
            if (s[i][j] = '.') and (s[i][j+1] = '.') then union1(ij(i, j), ij(i, j+1));

    for i := 1 to h-1 do
        for j := 1 to w do
            if (s[i][j] = '.') and (s[i+1][j] = '.') then union1(ij(i, j), ij(i+1, j));

    for i := 1 to h do
        for j := 1 to w do
            if s[i][j] = '1' then begin
                if (j > 1) and (s[i][j-1] = '.') then union1(ij(i, j), ij(i, j-1));
                if (j < w) and (s[i][j+1] = '.') then union1(ij(i, j), ij(i, j+1));
                if (i > 1) and (s[i-1][j] = '.') then union1(ij(i, j), ij(i-1, j));
                if (i < h) and (s[i+1][j] = '.') then union1(ij(i, j), ij(i+1, j));
            end;

    mx := 0;
    for i := 1 to h do
        for j := 1 to w do
            mx := max(mx, size[ij(i, j)]);

    writeln(mx);
end.

```
