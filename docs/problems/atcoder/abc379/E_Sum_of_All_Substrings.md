# Problem: E_Sum_of_All_Substrings.pas

```pascal
program E_Sum_of_All_Substrings;
uses
    math;
const
    nn = 200 * 1000 + 1;
    tt = 512 * 1024;
var
    n, q, k, i, l, r, mx: int32;
    h, d: array [1 .. nn] of int32;
    t: array [1 .. tt] of int32;

procedure build(v, vl, vr: int32);
var
    m: int32;
begin
    if vl = vr then
        t[v] := h[vl]
    else begin
        m := (vl+vr) div 2;
        build(2*v, vl, m);
        build(2*v+1, m+1, vr);
        t[v] := max(t[2*v], t[2*v+1]);
    end;
end;

function rmq(v, vl, vr, l, r: int32): int32;
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
        rmq := 0
    else if (l <= vl) and (vr <= r) then
        rmq := t[v]
    else begin
        m := (vl+vr) div 2;
        rmq := max(
            rmq(2*v, vl, m, l, r),
            rmq(2*v+1, m+1, vr, l, r)
        );
    end;
end;

function mnr(l0, h0: int32): int32;
var
    m, r: int32;
begin
    l := l0-1;
    r := n+1;
    while r-l > 1 do begin
        m := (l+r) div 2;
        if rmq(1, 1, n, l0, m) < h0 then
            l := m
        else
            r := m;
    end;
    mnr := r;
end;

begin
    readln(n, q);

    for i := 1 to n do read(h[i]);
    readln;
    build(1, 1, n);

    h[n+1] := high(int32);
    d[n+1] := 0;
    for i := n downto 1 do
        d[i] := d[mnr(i+1, h[i])] + 1;

    for k := 1 to q do begin

        readln(l, r);
        mx := rmq(1, 1, n, l+1, r);
        writeln(d[mnr(r+1, mx)]);

    end;
end.

```
