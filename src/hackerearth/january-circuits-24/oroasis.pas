program oroasis;
const
    maxn = 1000 * 1000 + 1;
    maxt = 2048 * 1024;
var
    ntc, n, i, l, r, or_in, or_ou, d, c: int32;
    a: array [1 .. maxn] of int32;
    st: array [1 .. maxt] of int32;
 
procedure combine(v: int32);
begin
    st[v] := st[2*v] or st[2*v+1];
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
        query := 0
    else if (l <= vl) and (vr <= r) then
        query := st[v]
    else {if vl < vr then} begin
        m := (vl+vr) div 2;
        query :=
            query(2*v, vl, m, l, r) or
            query(2*v+1, m+1, vr, l, r);
    end;
end;
 
begin
    readln(ntc);
    repeat
 
        readln(n);
        for i := 1 to n do read(a[i]); readln;
        build(1, 1, n);
 
        l := 1;
        r := 0;
        d := n+1;
 
        while r <= n do begin
 
            or_in := query(1, 1, n, l, r);
            or_ou := query(1, 1, n, 1, l-1) or query(1, 1, n, r+1, n);
 
            if or_in < or_ou then
                inc(r)
            else begin
 
                if or_in > or_ou then
                    (* pass *)
                else if d > r-l+1 then begin
                    d := r-l+1;
                    c := 1;
                end else if d = r-l+1 then
                    inc(c);
                inc(l);
 
            end;
 
        end;
 
        if d <= n then
            writeln(d, ' ', c)
        else
            writeln(-1);
 
        dec(ntc);
    until ntc = 0;
end.
