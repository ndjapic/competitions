# Problem: F_Two_Sequence_Queries.pas

```pascal
program F_Two_Sequence_Queries;
const
    nn = 200 * 1000;
    tt = 512 * 1024;
    prime = 998244353;
var
    n, q, i, l, r, x, y: int32;
    tp: int8;
    a, b: array [1 .. nn] of int32;
    st, lz: array [1 .. tt] of record
        a, b, ab: int32;
    end;

procedure combine(v: int32);
begin
    st[v].a := (st[2*v].a + st[2*v+1].a) mod prime;
    st[v].b := (st[2*v].b + st[2*v+1].b) mod prime;
    st[v].ab := (st[2*v].ab + st[2*v+1].ab) mod prime;
end;

procedure push(v, l, m, r: int32);
begin
    st[2*v].ab := (st[2*v].ab + int64(st[2*v].b) * lz[v].a) mod prime;
    st[2*v+1].ab := (st[2*v+1].ab + int64(st[2*v+1].b) * lz[v].a) mod prime;

    st[2*v].a := (st[2*v].a + int64(m-l+1) * lz[v].a) mod prime;
    st[2*v].b := (st[2*v].b + int64(m-l+1) * lz[v].b) mod prime;
    st[2*v+1].a := (st[2*v+1].a + int64(r-m) * lz[v].a) mod prime;
    st[2*v+1].b := (st[2*v+1].b + int64(r-m) * lz[v].b) mod prime;

    st[2*v].ab := (st[2*v].ab + int64(st[2*v].a) * lz[v].b) mod prime;
    st[2*v+1].ab := (st[2*v+1].ab + int64(st[2*v+1].a) * lz[v].b) mod prime;

    lz[2*v].a := (lz[2*v].a + lz[v].a) mod prime;
    lz[2*v].b := (lz[2*v].b + lz[v].b) mod prime;
    lz[2*v+1].a := (lz[2*v+1].a + lz[v].a) mod prime;
    lz[2*v+1].b := (lz[2*v+1].b + lz[v].b) mod prime;

    lz[v].a := 0;
    lz[v].b := 0;
end;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    lz[v].a := 0;
    lz[v].b := 0;
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
        combine(v);
    end else begin
        st[v].a := a[l];
        st[v].b := b[l];
        st[v].ab := int64(a[l]) * b[l] mod prime;
    end;
end;

procedure update(v, vl, vr, l, r, x, y: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
    else if (l <= vl) and (vr <= r) then begin

        st[v].ab := (st[v].ab + int64(st[v].b) * x) mod prime;

        st[v].a := (st[v].a + int64(vr-vl+1) * x) mod prime;
        st[v].b := (st[v].b + int64(vr-vl+1) * y) mod prime;

        st[v].ab := (st[v].ab + int64(st[v].a) * y) mod prime;

        lz[v].a := (lz[v].a + x) mod prime;
        lz[v].b := (lz[v].b + y) mod prime;

    end else {if vl < vr then} begin

        m := (vl+vr) div 2;
        push(v, vl, m, vr);
        update(2*v, vl, m, l, r, x, y);
        update(2*v+1, m+1, vr, l, r, x, y);
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
        query := st[v].ab
    else {if vl < vr then} begin
        m := (vl+vr) div 2;
        push(v, vl, m, vr);
        query := (
            query(2*v, vl, m, l, r) +
            query(2*v+1, m+1, vr, l, r)
        ) mod prime;
    end;
end;

begin
    readln(n, q);

    for i := 1 to n do read(a[i]); readln;
    for i := 1 to n do read(b[i]); readln;

    build(1, 1, n);

    for i := 1 to q do begin
        read(tp, l, r);
        case tp of

            1: begin
                read(x);
                update(1, 1, n, l, r, x, 0);
            end;

            2: begin
                read(y);
                update(1, 1, n, l, r, 0, y);
            end;

            3: writeln(query(1, 1, n, l, r));

        end;
        readln;
    end;
end.

```
