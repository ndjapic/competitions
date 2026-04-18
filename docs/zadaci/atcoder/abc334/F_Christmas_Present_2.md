# Задатак: F_Christmas_Present_2.pas

```pascal
program F_Christmas_Present_2;
const
    maxn = 200 * 1000;
type
    tpoint = record
        x, y: int64;
    end;
var
    n, k, i: int32;
    s: tpoint;
    ans: extended;
    h: array [0 .. maxn] of tpoint;
    d: array [0 .. maxn] of extended;
    pq: record
        a: array [1 .. maxn] of int32;
        n: int32;
    end;

function prior(x, y: int32): boolean;
begin
    prior := d[x] < d[y];
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

function dist(a, b: tpoint): extended;
begin
    dist := sqrt(sqr(a.x - b.x) + sqr(a.y - b.y));
end;

begin
    readln(n, k);
    for i := 0 to n do readln(h[i].x, h[i].y);

    s := h[0];
    h[0] := h[n];
    ans := 0;
    pq.n := 0;
    d[0] := dist(h[0], s) + dist(s, h[1]) - dist(h[0], h[1]);
    pqins(pq.n+1, 0);

    for i := 1 to n do begin
        d[i-1] := d[i-1] + dist(h[i], s) + dist(s, h[i+1]) - dist(h[i], h[i+1]);
        pqins(pq.n+1, i-1);
        ans := ans + dist(h[i-1], h[i]);
        while pq.a[1] < i-k do pqdel(1);
        d[i] := d[pq.a[1]];
    end;

    ans := ans + d[n];
    writeln(ans:24:6);
end.

```
