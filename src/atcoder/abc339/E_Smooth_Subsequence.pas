program E_Smooth_Subsequence;
uses
    math;
const
    maxn = 500 * 1000;
    maxt = 1024 * 1024;
var
    n, d, i, ai, current, best: int32;
    st: array [1 .. maxt] of int32;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
    end;
    st[v] := 0;
end;

procedure update(v, vl, vr, i, x: int32);
var
    m: int32;
begin
    if (i < vl) or (vr < i) then
    else if vl = vr then
        st[v] := max(st[v], x)
    else {if vl < vr then} begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, i, x);
        update(2*v+1, m+1, vr, i, x);
        st[v] := max(st[2*v], st[2*v+1]);
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
        query := max(
            query(2*v, vl, m, l, r),
            query(2*v+1, m+1, vr, l, r)
        );
    end;
end;

begin
    build(1, 1, maxn);
    best := 0;

    readln(n, d);

    for i := 1 to n do begin
        read(ai);
        current := query(1, 1, maxn, ai-d, ai+d) + 1;
        best := max(best, current);
        update(1, 1, maxn, ai, current);
    end;
    readln;

    writeln(best);
end.
