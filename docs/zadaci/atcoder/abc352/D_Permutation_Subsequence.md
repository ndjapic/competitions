# Задатак: D_Permutation_Subsequence.pas

```pascal
program D_Permutation_Subsequence;
uses
    math;
const
    nn = 200 * 1000;
    maxt = 512 * 1024;
var
    n, k, i, a, b, ans: int32;
    p, inv: array [1 .. nn] of int32;
    st: array [1 .. maxt] of boolean;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    st[v] := false;
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
    end;
end;

procedure update(v, vl, vr, i: int32; bit: boolean);
var
    m: int32;
begin
    if (i < vl) or (vr < i) then
    else if (i <= vl) and (vr <= i) then begin
        st[v] := bit;
    end else begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, i, bit);
        update(2*v+1, m+1, vr, i, bit);
        st[v] := st[2*v] or st[2*v+1];
    end;
end;

function query_min(v, vl, vr: int32): int32;
var
    m: int32;
begin
    m := (vl+vr) div 2;
    if vl = vr then
        query_min := vl
    else if st[2*v] then
        query_min := query_min(2*v, vl, m)
    else
        query_min := query_min(2*v+1, m+1, vr);
end;

function query_max(v, vl, vr: int32): int32;
var
    m: int32;
begin
    m := (vl+vr) div 2;
    if vl = vr then
        query_max := vr
    else if st[2*v+1] then
        query_max := query_max(2*v+1, m+1, vr)
    else
        query_max := query_max(2*v, vl, m);
end;

begin
    readln(n, k);

    for i := 1 to n do begin
        read(p[i]);
        inv[p[i]] := i;
    end;
    readln;

    build(1, 1, n);
    for i := 1 to k-1 do update(1, 1, n, inv[i], true);

    a := 1;
    ans := n-1;
    for b := k to n do begin
        update(1, 1, n, inv[b], true);
        ans := min(ans, query_max(1, 1, n) - query_min(1, 1, n));
        update(1, 1, n, inv[a], false);
        inc(a);
    end;

    writeln(ans);
end.

```
