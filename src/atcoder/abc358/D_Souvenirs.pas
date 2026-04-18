program D_Souvenirs;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    n, m, i, j: int32;
    ans: int64;
    a, b: array [1 .. nn] of int32;
    pq: record
        a: array [1 .. nn] of int32;
        n: int32;
    end;

function prior(l, r: int32): boolean;
begin
    prior := l < r;
end;

procedure pqins(v, x: int32);
(* Usage: pqins(pq.n+1, x); *)
var
    u: int32;
begin
    u := v div 2;
    if (v > 1) and prior(x, pq.a[u]) then begin
        pq.a[v] := pq.a[u];
        pqins(u, x);
    end else begin
        pq.a[v] := x;
        inc(pq.n);
    end;
end;

procedure pqdel(u: int32);
(* Usage: pqdel(1); *)
var
    v: int32;
begin
    v := u * 2;
    if (v+1 <= pq.n-1) and prior(pq.a[v+1], pq.a[v]) then inc(v);
    if (v <= pq.n-1) and prior(pq.a[v], pq.a[pq.n]) then begin
        pq.a[u] := pq.a[v];
        pqdel(v);
    end else begin
        pq.a[u] := pq.a[pq.n];
        dec(pq.n);
    end;
end;

begin
    readln(n, m);

    pq.n := 0;
    for i := 1 to n do begin
        read(a[i]);
        pqins(1+pq.n, a[i]);
    end;
    readln;

    for i := 1 to n do begin
        a[i] := pq.a[1];
        pqdel(1);
    end;

    pq.n := 0;
    for j := 1 to m do begin
        read(b[j]);
        pqins(1+pq.n, b[j]);
    end;
    readln;

    for j := 1 to m do begin
        b[j] := pq.a[1];
        pqdel(1);
    end;

    i := 1;
    j := 1;
    ans := 0;
    while (i <= n) and (j <= m) do begin
        if a[i] >= b[j] then begin
            inc(ans, a[i]);
            inc(j);
        end;
        inc(i);
    end;

    if j <= m then ans := -1;
    writeln(ans);
end.
