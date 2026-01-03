program Zadatak_AN2DL;
uses
    math;
const
    maxn = 4000;
    maxt = 8192;
var
    n, m, i, j, r, s: int16;
    a, b, c: array [1 .. maxn, 1 .. maxn] of int16;
    x: array [1 .. maxn] of int16;
    t: array [1 .. maxt] of int16;

procedure build(v, l, r: int16);
var
    m: int16;
begin
    if l < r then begin
        m := (l+r) div 2;
        build(2*v, l, m);
        build(2*v+1, m+1, r);
        t[v] := max(t[2*v], t[2*v+1]);
    end else
        t[v] := x[l];
end;

function query(v, vl, vr, l, r: int16): int16;
var
    m: int16;
begin
    if (r < vl) or (vr < l) then
        query := low(int16)
    else if (l <= vl) and (vr <= r) then
        query := t[v]
    else begin
        m := (vl+vr) div 2;
        query := max(
            query(2*v, vl, m, l, r),
            query(2*v+1, m+1, vr, l, r)
        );
    end;
end;

begin
    readln(n, m);

    for i := 1 to n do begin
        for j := 1 to m do read(a[i, j]);
        readln;
    end;

    readln(r, s);

    for i := 1 to n do begin
        for j := 1 to m do x[j] := a[i, j];
        build(1, 1, m);
        for j := s to m do b[i, j] := query(1, 1, m, j-s+1, j);
    end;

    for j := s to m do begin
        for i := 1 to n do x[i] := b[i, j];
        build(1, 1, n);
        for i := r to n do c[i, j] := query(1, 1, n, i-r+1, i);
    end;

    for i := r to n do begin
        for j := s to m-1 do write(c[i, j], ' ');
        writeln(c[i, m]);
    end;
end.
