# Задатак: E_Iva_Pav.pas

```pascal
program E_Iva_Pav;
uses
    math;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    ntc, tci: int16;
    n, i, q, j, l, r, k, lo, hi: int32;
    a: array [1 .. maxn] of int32;
    st: array [1 .. maxt] of int32;

procedure combine(v: int32);
begin
    st[v] := st[2*v] and st[2*v+1];
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

function query(v, vl, vr, l, r: int32): int32;
var
    m: int32;
begin
    if (r < vl) or (vr < l) then
        query := (int32(1) shl 30) - 1
    else if (l <= vl) and (vr <= r) then
        query := st[v]
    else {if l < r then} begin
        m := (vl+vr) div 2;
        query :=
            query(2*v, vl, m, l, r) and
            query(2*v+1, m+1, vr, l, r);
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        build(1, 1, n);

        readln(q);
        for j := 1 to q do begin

            readln(l, k);

            if a[l] < k then
                write('-1 ')
            else begin

                lo := l;
                hi := n+1;
                while hi - lo > 1 do begin

                    r := (lo + hi) div 2;
                    if query(1, 1, n, l, r) < k then
                        hi := r
                    else
                        lo := r;

                end;
                write(lo, ' ');

            end;

        end;
        writeln;

    end;
end.


```
