# Problem: F_Transpose.pas

```pascal
program F_Transpose;
{$H+}
const
    szs = 500 * 1000;
    szt = 1024 * 1024;
var
    n, x, y, h, a, b: int32;
    s, t: string;
    l: array [1 .. szs] of int32;
    st, lz, qt: array [1 .. szt] of record
        a, b: int32;
    end;

procedure combine(v: int32);
begin
    qt[v].b := qt[2*v+1].a * qt[2*v].b + qt[2*v+1].b;
    qt[v].a := qt[2*v+1].a * qt[2*v].a;
end;

procedure push(v: int32);
begin
    st[2*v].b := lz[v].a * st[2*v].b + lz[v].b;
    st[2*v].a := lz[v].a * st[2*v].a;
    st[2*v+1].b := lz[v].a * st[2*v+1].b + lz[v].b;
    st[2*v+1].a := lz[v].a * st[2*v+1].a;

    lz[2*v].b := lz[v].a * lz[2*v].b + lz[v].b;
    lz[2*v].a := lz[v].a * lz[2*v].a;
    lz[2*v+1].b := lz[v].a * lz[2*v+1].b + lz[v].b;
    lz[2*v+1].a := lz[v].a * lz[2*v+1].a;

    lz[v].b := 0;
    lz[v].a := 1;
end;

procedure build(v, l, r: int32);
var
    m: int32;
begin
    st[v].b := 0;
    st[v].a := 1;
    lz[v].b := 0;
    lz[v].a := 1;
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
    end;
end;

procedure update(v, vl, vr, l, r, a, b: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
    else if (l <= vl) and (vr <= r) then begin
        st[v].b := a * st[v].b + b;
        st[v].a := a * st[v].a;
        lz[v].b := a * lz[v].b + b;
        lz[v].a := a * lz[v].a;
    end else {if vl < vr then} begin
        push(v);
        m := (vl+vr) div 2;
        update(2*v, vl, m, l, r, a, b);
        update(2*v+1, m+1, vr, l, r, a, b);
    end;
end;

procedure query(v, vl, vr, x: int32);
var
    m: int32;
begin
    if (x < vl) or (vr < x) then begin
        qt[v].b := 0;
        qt[v].a := 1;
    end else if (x <= vl) and (vr <= x) then begin
        qt[v] := st[v];
    end else {if vl < vr then} begin
        push(v);
        m := (vl+vr) div 2;
        query(2*v, vl, m, x);
        query(2*v+1, m+1, vr, x);
        combine(v);
    end;
end;

begin
    readln(s);
    n := length(s);
    setlength(t, n);
    build(1, 1, n);

    h := 0;
    for x := 1 to n do
        if s[x] = '(' then begin
            inc(h);
            l[h] := x;
        end else if s[x] = ')' then begin
            update(1, 1, n, l[h]+1, x-1, -1, x+l[h]);
            dec(h);
        end;

    for x := 1 to n do begin
        query(1, 1, n, x);
        a := qt[1].a;
        b := qt[1].b;
        y := a * x + b;
        if a = -1 then begin
            if (s[x] >= 'A') and (s[x] <= 'Z') then
                s[x] := chr(ord(s[x]) + 32)
            else if (s[x] >= 'a') and (s[x] <= 'z') then
                s[x] := chr(ord(s[x]) - 32);
        end;
        {writeln(' x=',x, ' s=',s[x], ' y=',y);}
        t[y] := s[x];
    end;

    for y := 1 to n do
        if (t[y] <> '(') and (t[y] <> ')') then write(t[y]);
    writeln;
end.

```
