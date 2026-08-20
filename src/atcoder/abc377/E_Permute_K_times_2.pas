program E_Permute_K_times_2;
{$mode objfpc}{$h+}{$j-}{$inline on}
uses
    math;
const
    nn = 200 * 1000;
    ee = 17;
var
    n, v, m, x, p2: int32;
    k: int64;
    e: int8;
    p: array [0 .. ee, 1 .. nn] of int32;
    dsu, size: array [1 .. nn] of int32;

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

function modmul(m, a, b: int32): int32;
begin
    modmul := int64(a) * b mod m;
end;

function modpow(m, b: int32; k: int64): int32;
begin
    if k = 0 then
        modpow := 1
    else if odd(k) then
        modpow := modmul(m, modpow(m, b, k-1), b)
    else
        modpow := modpow(m, modmul(m, b, b), k div 2);
end;

begin
    readln(n, k);

    for v := 1 to n do begin
        dsu[v] := v;
        size[v] := 1;
    end;

    for v := 1 to n do begin
        read(p[0, v]);
        union1(v, p[0, v]);
    end;
    readln;

    for e := 0 to ee-1 do
        for v := 1 to n do
            p[e+1, v] := p[e, p[e, v]];

    for v := 1 to n do begin
        m := modpow(size[find(v)], 2, k);
        x := v;
        p2 := int32(1) shl ee;
        for e := ee downto 0 do begin
            if m >= p2 then begin
                x := p[e, x];
                dec(m, p2);
            end;
            p2 := p2 div 2;
        end;
        write(x, ' ');
    end;
    writeln;
end.
