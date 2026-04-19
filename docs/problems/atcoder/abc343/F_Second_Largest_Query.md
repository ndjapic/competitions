# Problem: F_Second_Largest_Query.pas

```pascal
program F_Second_Largest_Query;
uses
    math;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    n, q, i, k, p, x, l, r: int32;
    tp: int8;
    a: array [1 .. maxn] of int32;
    st, qt: array [1 .. maxt] of record
        x, c: array [1 .. 2] of int32;
    end;

procedure scombine(v: int32);
var
    i, l, r: int8;
begin
    l := 1;
    r := 1;
    for i := 1 to 2 do begin
        st[v].x[i] := max(st[2*v].x[l], st[2*v+1].x[r]);
        st[v].c[i] := 0;
        if st[2*v].x[l] = st[v].x[i] then begin
            inc(st[v].c[i], st[2*v].c[l]);
            inc(l);
        end;
        if st[2*v+1].x[r] = st[v].x[i] then begin
            inc(st[v].c[i], st[2*v+1].c[r]);
            inc(r);
        end;
    end;
end;

procedure qcombine(v: int32);
var
    i, l, r: int8;
begin
    l := 1;
    r := 1;
    for i := 1 to 2 do begin
        qt[v].x[i] := max(qt[2*v].x[l], qt[2*v+1].x[r]);
        qt[v].c[i] := 0;
        if qt[2*v].x[l] = qt[v].x[i] then begin
            inc(qt[v].c[i], qt[2*v].c[l]);
            inc(l);
        end;
        if qt[2*v+1].x[r] = qt[v].x[i] then begin
            inc(qt[v].c[i], qt[2*v+1].c[r]);
            inc(r);
        end;
    end;
end;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
        scombine(v);
    end else begin
        st[v].x[1] := a[l];
        st[v].c[1] := 1;
        st[v].x[2] := 0;
        st[v].c[2] := 0;
    end;
end;

procedure update(v, vl, vr, p: int32);
var
    m: int32;
begin
    if (p < vl) or (vr < p) then
    else if (p <= vl) and (vr <= p) then begin
        st[v].x[1] := a[p];
        st[v].c[1] := 1;
        st[v].x[2] := 0;
        st[v].c[2] := 0;
    end else {if vl < vr then} begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, p);
        update(2*v+1, m+1, vr, p);
        scombine(v);
    end;
end;

procedure query(v, vl, vr, l, r: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then begin
        qt[v].x[1] := 0;
        qt[v].c[1] := 0;
        qt[v].x[2] := 0;
        qt[v].c[2] := 0;
    end else if (l <= vl) and (vr <= r) then begin
        qt[v] := st[v];
    end else {if vl < vr then} begin
        m := (vl+vr) div 2;
        query(2*v, vl, m, l, r);
        query(2*v+1, m+1, vr, l, r);
        qcombine(v);
    end;
end;

begin
    readln(n, q);

    for i := 1 to n do read(a[i]); readln;

    build(1, 1, n);

    for k := 1 to q do begin
        read(tp);
        case tp of

            1: begin
                readln(p, x);
                a[p] := x;
                update(1, 1, n, p);
            end;

            2: begin
                readln(l, r);
                query(1, 1, n, l, r);
                writeln(qt[1].c[2]);
            end;
        
        end;
    end;
end.

```
