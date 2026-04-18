program E_Mancala_2;
uses
    math;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    n, m, i, k: int32;
    ai: int64;
    a: array [0 .. maxn] of int64;
    st, lz: array [1 .. maxt] of int64;

procedure combine(v: int32);
begin
    st[v] := st[2*v] + st[2*v+1];
end;

procedure push(v: int32);
begin
    inc(st[2*v], lz[v]);
    inc(st[2*v+1], lz[v]);
    inc(lz[2*v], lz[v]);
    inc(lz[2*v+1], lz[v]);
    lz[v] := 0;
end;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    lz[v] := 0;
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
        combine(v);
    end else
        st[v] := a[l];
end;

procedure update(v, vl, vr, l, r: int32; d: int64);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
    else if (l <= vl) and (vr <= r) then begin
        inc(st[v], d);
        inc(lz[v], d);
    end else {if vl < vr then} begin
        push(v);
        m := (vl+vr) div 2;
        update(2*v, vl, m, l, r, d);
        update(2*v+1, m+1, vr, l, r, d);
        combine(v);
    end;
end;

function query(v, vl, vr, i: int32): int64;
var
    m: int32;
begin
    if (i < vl) or (vr < i) then
        query := 0
    else if (i <= vl) and (vr <= i) then
        query := st[v]
    else {if vl < vr then} begin
        push(v);
        m := (vl+vr) div 2;
        query :=
            query(2*v, vl, m, i) +
            query(2*v+1, m+1, vr, i);
    end;
end;

begin
    readln(n, m);

    for i := 0 to n-1 do read(a[i]); readln;

    build(1, 0, n-1);

    for k := 1 to m do begin
        read(i);
        ai := query(1, 0, n-1, i);
        update(1, 0, n-1, i, i, -ai);
        update(1, 0, n-1, 0, n-1, ai div n);
        ai := ai mod n;
        update(1, 0, n-1, i+1, min(i+ai, n-1), 1);
        dec(ai, n-1-i);
        if ai > 0 then
            update(1, 0, n-1, 0, ai-1, 1);
    end;
    readln;

    for i := 0 to n-2 do write(query(1, 0, n-1, i), ' ');
    writeln(query(1, 0, n-1, n-1));
end.
