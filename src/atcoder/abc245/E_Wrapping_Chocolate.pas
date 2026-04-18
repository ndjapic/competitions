program E_Wrapping_Chocolate;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    n, m, i, j, k, l, r, s: int32;
    ans: boolean;
    a, b, c, d, p, x_merge, y_merge, p_merge: array [1 .. maxn] of int32;
    st: array [1 .. maxt] of int32;

procedure msortb(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msortb(l, m);
        msortb(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                b[j] >= b[k]
            ) then begin
                x_merge[i] := a[j];
                y_merge[i] := b[j];
                inc(j);
            end else begin
                x_merge[i] := a[k];
                y_merge[i] := b[k];
                inc(k);
            end;

        for i := l to r-1 do begin
            a[i] := x_merge[i];
            b[i] := y_merge[i];
        end

    end;
end;

procedure msortc(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msortc(l, m);
        msortc(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                c[j] <= c[k]
            ) then begin
                x_merge[i] := c[j];
                y_merge[i] := d[j];
                inc(j);
            end else begin
                x_merge[i] := c[k];
                y_merge[i] := d[k];
                inc(k);
            end;

        for i := l to r-1 do begin
            c[i] := x_merge[i];
            d[i] := y_merge[i];
        end

    end;
end;

procedure msortd(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msortd(l, m);
        msortd(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                d[p[j]] >= d[p[k]]
            ) then begin
                p_merge[i] := p[j];
                inc(j);
            end else begin
                p_merge[i] := p[k];
                inc(k);
            end;

        for i := l to r-1 do p[i] := p_merge[i];

    end;
end;

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
    end;
    st[v] := 0;
end;

procedure update(v, vl, vr, j, active: int32);
var
    m: int32;
begin
    if (j < vl) or (vr < j) then
    else if (j <= vl) and (vr <= j) then begin
        st[v] := active;
    end else begin
        m := (vl+vr) div 2;
        update(2*v, vl, m, j, active);
        update(2*v+1, m+1, vr, j, active);
        combine(v);
    end;
end;

function query(v, vl, vr, k: int32): int32;
var
    m: int32;
begin
    if k < vl then
        query := 0
    else if vr <= k then
        query := st[v]
    else begin
        m := (vl+vr) div 2;
        query :=
            query(2*v, vl, m, k) +
            query(2*v+1, m+1, vr, k);
    end;
end;

begin
    readln(n, m);

    for i := 1 to n do read(a[i]); readln;
    for i := 1 to n do read(b[i]); readln;
    for j := 1 to m do read(c[j]); readln;
    for j := 1 to m do read(d[j]); readln;
    for j := 1 to m do p[j] := j;

    msortb(1, n+1);
    msortc(1, m+1);
    msortd(1, m+1);
    build(1, 1, m);

    ans := true;
    i := 1;
    j := 1;
    while ans and (i <= n) do begin

        while (j <= m) and (d[p[j]] >= b[i]) do begin
            update(1, 1, m, p[j], 1);
            inc(j);
        end;

        l := 0;
        r := m+1;
        while r-l > 1 do begin
            k := (l+r) div 2;
            if c[k] < a[i] then
                l := k
            else
                r := k;
        end;

        s := query(1, 1, m, l);
        r := m+1;
        while r-l > 1 do begin
            k := (l+r) div 2;
            if query(1, 1, m, k) <= s then
                l := k
            else
                r := k;
        end;

        ans := r <= m;
        if ans then
            update(1, 1, m, r, 0);

        inc(i);
    end;

    if ans then
        writeln('Yes')
    else
        writeln('No');

end.
