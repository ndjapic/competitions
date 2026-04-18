# Задатак: C_Peak.pas

```pascal
program C_Peak;
uses
    math;
const
    maxn = 300 * 1000;
var
    n, m, l, r, ans: int32;
    a: array [1 .. maxn] of int32;
    pq: record
        a: array [1 .. maxn] of int32;
        n: int32;
    end;

function prior(x, y: int32): boolean;
begin
    prior := x < y;
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

    for r := 1 to n do begin
        read(a[r]);
        pqins(pq.n+1, a[r]);
    end;
    readln;

    ans := 0;
    l := 1;
    for r := 1 to n do begin
        a[r] := pq.a[1];
        pqdel(1);
        while a[r] - a[l] >= m do inc(l);
        ans := max(ans, r-l+1);
    end;

    writeln(ans);
end.

```
