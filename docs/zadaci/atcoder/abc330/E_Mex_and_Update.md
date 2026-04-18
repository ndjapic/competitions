# Задатак: E_Mex_and_Update.pas

```pascal
program E_Mex_and_Update;
uses
    math;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    n, q, i, j, x, l, r, m: int32;
    a: array [1 .. maxn] of int32;
    c: array [0 .. maxn] of int32;
    st: array [1 .. maxt] of int32;

procedure combine(v: int32);
begin
    st[v] := st[2*v] + st[2*v+1];
end;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
        combine(v);
    end else
        st[v] := min(c[l], 1);
end;

procedure update(v, vl, vr, x: int32);
var
    m: int32;
begin
    if (x < vl) or (vr < x) then
    else if (x <= vl) and (vr <= x) then begin
        st[v] := min(c[x], 1);
    end else {if vl < vr then} begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, x);
        update(2*v+1, m+1, vr, x);
        combine(v);
    end;
end;

function query(v, vl, vr, l, r: int32): int32;
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
        query := 0
    else if (l <= vl) and (vr <= r) then
        query := st[v]
    else {if vl < vr then} begin
        m := (vl+vr) div 2;
        query :=
            query(2*v, vl, m, l, r) +
            query(2*v+1, m+1, vr, l, r);
    end;
end;

begin
    readln(n, q);

    for x := 0 to n do c[x] := 0;

    for i := 1 to n do begin
        read(x);
        a[i] := x;
        if x <= n then inc(c[x]);
    end;
    readln;

    build(1, 0, n);

    for j := 1 to q do begin

        readln(i, x);

        if a[i] <= n then begin
            dec(c[a[i]]);
            update(1, 0, n, a[i]);
        end;

        if x <= n then begin
            inc(c[x]);
            update(1, 0, n, x);
        end;

        a[i] := x;

        l := 0;
        r := n+1;
        while r-l > 1 do begin
            m := (l+r) div 2;
            if query(1, 0, n, 0, m-1) = m then
                l := m
            else
                r := m;
        end;

        writeln(l);

    end;
end.

```
