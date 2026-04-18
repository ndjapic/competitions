# Задатак: D_Find_the_Different_Ones.pas

```pascal
program D_Find_the_Different_Ones;
uses
    math;
const
    maxn = 200 * 1000;
    maxt = 512 * 1024;
var
    ntc, tci: int16;
    n, q, i, j, k, l, r: int32;
    a: array [1 .. maxn] of int32;
    st, qt: array [1 .. maxt] of record
        i, j: int32;
    end;

procedure scombine(v: int32);
begin
    if a[st[2*v].i] <= a[st[2*v+1].i] then
        st[v].i := st[2*v].i
    else
        st[v].i := st[2*v+1].i;
    if a[st[2*v].j] >= a[st[2*v+1].j] then
        st[v].j := st[2*v].j
    else
        st[v].j := st[2*v+1].j;
end;

procedure qcombine(v: int32);
begin
    if (qt[2*v+1].i = -1) or (qt[2*v].i > -1) and (a[qt[2*v].i] <= a[qt[2*v+1].i]) then
        qt[v].i := qt[2*v].i
    else
        qt[v].i := qt[2*v+1].i;
    if (qt[2*v+1].j = -1) or (qt[2*v].j > -1) and (a[qt[2*v].j] >= a[qt[2*v+1].j]) then
        qt[v].j := qt[2*v].j
    else
        qt[v].j := qt[2*v+1].j;
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
        st[v].i := l;
        st[v].j := l;
    end;
end;

procedure query(v, vl, vr, l, r: int32);
var
    m: int32;
begin
    if (r < vl) or (vr < l) then begin
        qt[v].i := -1;
        qt[v].j := -1;
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
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;
        build(1, 1, n);

        readln(q);
        for k := 1 to q do begin

            readln(l, r);
            query(1, 1, n, l, r);

            i := qt[1].i;
            j := qt[1].j;

            if i = j then begin
                i := -1;
                j := -1;
            end;

            writeln(i, ' ', j);

        end;

        if tci < ntc then writeln;

    end;
end.

```
