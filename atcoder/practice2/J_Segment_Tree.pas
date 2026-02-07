program J_Segment_Tree;
uses
    math;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    n, q, i, mn, l, r, m: int32;
    tp: int8;
    a: array [1 .. maxn] of int32;
    st: array [1 .. maxt] of int32;

procedure combine(v: int32);
begin
    st[v] := max(st[2*v], st[2*v+1]);
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
        st[v] := a[l];
end;

procedure update(v, vl, vr, l, r: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
    else if (l <= vl) and (vr <= r) then begin
        st[v] := a[l];
    end else {if vl < vr then} begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, l, r);
        update(2*v+1, m+1, vr, l, r);
        combine(v);
    end;
end;

function query2(v, vl, vr, l, r: int32): int32;
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
        query2 := 0
    else if (l <= vl) and (vr <= r) then
        query2 := st[v]
    else {if vl < vr then} begin
        m := (vl+vr) div 2;
        query2 := max(
            query2(2*v, vl, m, l, r),
            query2(2*v+1, m+1, vr, l, r)
        );
    end;
end;

begin
    readln(n, q);

    for i := 1 to n do read(a[i]); readln;

    build(1, 1, n);

    for i := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                read(l);
                readln(a[l]);
                update(1, 1, n, l, l);
            end;

            2: begin
                readln(l, r);
                writeln(query2(1, 1, n, l, r));
            end;

            3: begin
                readln(l, mn);
                dec(l);
                r := n+1;

                while r-l > 1 do begin
                    m := (l+r) div 2;
                    if query2(1, 1, n, l+1, m) < mn then
                        l := m
                    else
                        r := m;
                end;

                writeln(r);
            end;
        
        end;
    end;
end.
