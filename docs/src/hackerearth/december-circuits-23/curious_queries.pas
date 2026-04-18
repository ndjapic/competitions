program curious_queries;
const
    maxn = 100 * 1000;
    maxt = 256 * 1024;
var
    ntc, tci: int8;
    n, q, i, j, k: int32;
    a, nq, r: array [0 .. maxn] of int32;
    st: array [1 .. maxt] of int64;
    queries: array [0 .. maxn] of array of int32;
    b: array [1 .. maxn] of int64;

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
        st[v] := 0;
end;

procedure update(v, vl, vr, x: int32);
var
    m: int32;
begin
    if (x < vl) or (vr < x) then
    else if (x <= vl) and (vr <= x) then begin
        inc(st[v], x);
    end else {if vl < vr then} begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, x);
        update(2*v+1, m+1, vr, x);
        combine(v);
    end;
end;

function query(v, vl, vr, l, r: int32): int64;
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
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n, q);

        for i := 0 to n-1 do read(a[i]); readln;

        build(1, 1, maxn);

        for i := 0 to n do begin
            setlength(queries[i], 1);
            nq[i] := 0;
        end;

        for k := 1 to q do begin
            readln(i, r[k]);
            if nq[i] = length(queries[i]) then setlength(queries[i], 2*nq[i]);
            queries[i][nq[i]] := k;
            inc(nq[i]);
        end;

        for i := 0 to n-1 do begin
            update(1, 1, maxn, a[i]);
            for j := 0 to nq[i]-1 do begin
                k := queries[i][j];
                b[k] := query(1, 1, maxn, a[r[k]]+1, maxn);
            end;
        end;

        for k := 1 to q-1 do write(b[k], ' ');
        writeln(b[q]);
    end;
end.
