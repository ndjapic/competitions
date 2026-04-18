# Задатак: G_Christmas_Color_Grid_2.pas

```pascal
program G_Christmas_Color_Grid_2;
{$H+}
uses
    math;
const
    maxh = 1000;
    maxn = maxh * maxh;
    prime = 998244353;
var
    h, w, n, i, j, v, c, green, num: int32;
    den: int64;
    top: int8;
    s: array [1 .. maxh] of string;
    dsu, size, preh, sufh: array [1 .. maxn] of int32;
    adj: array [1 .. 4] of int32;

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union2(u, v: int32);
begin
    dsu[v] := u;
    inc(size[u], size[v]);
    inc(c);
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

function ij(i, j: int32): int32;
begin
    ij := (i-1)*w + j;
end;

procedure push(i, j: int32);
var
    k: int8;
    v: int32;
begin
    if s[i][j] = '#' then begin

        v := find(ij(i, j));

        k := 1;
        while (k <= top) and (adj[k] <> v) do inc(k);

        if k > top then begin
            inc(top);
            adj[k] := v;
        end;

    end;
end;

function modpow(b, e: int32): int32;
begin
    if e = 0 then
        modpow := 1
    else if odd(e) then
        modpow := int64(b) * modpow(b, e-1) mod prime
    else
        modpow := modpow(int64(b) * b mod prime, e div 2);
end;

begin
    readln(h, w);

    n := h * w;
    for v := 1 to n do begin
        dsu[v] := v;
        size[v] := 1;
    end;

    c := 0;
    green := 0;
    for i := 1 to h do begin

        readln(s[i]);

        for j := 1 to w do
            if s[i][j] = '#' then inc(green);

        if i > 1 then
            for j := 1 to w do
                if (s[i][j] = '#') and (s[i-1][j] = '#') then union1(ij(i, j), ij(i-1, j));

        for j := 2 to w do
            if (s[i][j] = '#') and (s[i][j-1] = '#') then union1(ij(i, j), ij(i, j-1));

    end;

    den := 0;
    nom := 0;
    for i := 1 to h do
        for j := 1 to w do
            if s[i][j] = '.' then begin
                inc(nom);
                top := 0;
                if i > 1 then push(i-1, j);
                if i < h then push(i+1, j);
                if j > 1 then push(i, j-1);
                if j < w then push(i, j+1);
                inc(den, green - c - top + 1);
            end;

    writeln(den mod prime * modpow(nom, prime - 2) mod prime);
end.

```
