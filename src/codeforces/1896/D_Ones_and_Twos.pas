program D_Ones_and_Twos;
uses
    math;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    ntc, tci: int16;
    n, q, i, j, l, r, m, s, v: int32;
    op: int8;
    ans: boolean;
    a: array [1 .. maxn] of int8;
    st: array [1 .. maxt] of record
        s, m: int32;
    end;

procedure combine(v: int32);
begin
    st[v].s := st[2*v].s + st[2*v+1].s;
    st[v].m := min(st[2*v].m, st[2*v+1].m);
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
    end else begin
        st[v].s := a[l];
        st[v].m := a[l];
    end;
end;

procedure update(v, l, r, i: int32);
var
    m: int32;
begin
    if (i < l) or (r < i) then
    else if (i <= l) and (r <= i) then begin
        st[v].s := a[i];
        st[v].m := a[i];
    end else {if vl < vr then} begin
        m := (l+r) div 2;
        update(2*v, l, m, i);
        update(2*v+1, m+1, r, i);
        combine(v);
    end;
end;

function query_sum(v, vl, vr, l, r: int32): int32;
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
        query_sum := 0
    else if (l <= vl) and (vr <= r) then
        query_sum := st[v].s
    else {if vl < vr then} begin
        m := (vl+vr) div 2;
        query_sum :=
            query_sum(2*v, vl, m, l, r) +
            query_sum(2*v+1, m+1, vr, l, r);
    end;
end;

function query_min(v, vl, vr, l, r: int32): int32;
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
        query_min := high(int32)
    else if (l <= vl) and (vr <= r) then
        query_min := st[v].m
    else {if vl < vr then} begin
        m := (vl+vr) div 2;
        query_min := min(
            query_min(2*v, vl, m, l, r),
            query_min(2*v+1, m+1, vr, l, r)
        );
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, q);

        for i := 1 to n do read(a[i]);
        readln;

        build(1, 1, n);

        for j := 1 to q do begin

            read(op);
            case op of

                1: begin

                    readln(s);
                    ans := false;

                    if not ans then begin

                        l := 0;
                        r := n+1;
                        while r-l > 1 do begin
                            m := (l+r) div 2;
                            if query_sum(1, 1, n, 1, m) <= s then
                                l := m
                            else
                                r := m;
                        end;
                        ans := (query_sum(1, 1, n, 1, l) = s) or (r <= n) and (query_min(1, 1, n, r, n) < 2);

                    end;

                    if not ans then begin

                        l := 0;
                        r := n+1;
                        while r-l > 1 do begin
                            m := (l+r) div 2;
                            if query_sum(1, 1, n, m, n) <= s then
                                r := m
                            else
                                l := m;
                        end;
                        ans := (query_sum(1, 1, n, r, n) = s) or (l > 0) and (query_min(1, 1, n, 1, l) < 2);

                    end;

                    if ans then
                        writeln('YES')
                    else
                        writeln('NO');

                end;

                2: begin

                    readln(i, v);
                    a[i] := v;
                    update(1, 1, n, i);

                end;

            end;

        end;

    end;
end.
