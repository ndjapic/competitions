# Задатак: E_Last_Train.pas

```pascal
program E_Last_Train;
uses
    math;
const
    maxn = 200 * 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1001;
type
    pqelm = record
        b: int32; (* station *)
        t: int64; (* last time train *)
    end;
var
    n, m, i, b: int32;
    x, y: pqelm;
    adj: array [1 .. maxn] of int32;
    sib, tar: array [1 .. maxn] of int32;
    f, l, d, k, c: array [1 .. maxn] of int64;
    pq: record
        a: array of pqelm;
        n: int32;
    end;

procedure addarrow(u, v, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure readedges(n, m: int32);
var
    a, b, i: int32;
begin
    for b := 1 to n do begin
        adj[b] := 0;
        f[b] := 0;
    end;

    for i := 1 to m do begin
        readln(l[i], d[i], k[i], c[i], a, b);
        addarrow(b, a, i);
    end;
end;

function prior(x, y: pqelm): boolean;
begin
    prior := x.t > y.t;
end;

procedure pqins(v: int32; x: pqelm);
(* Usage: pqins(pq.n+1, x); *)
var
    u: int32;
begin
    if length(pq.a) <= v then setlength(pq.a, 2*v);
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
    readedges(n, m);

    pq.n := 0;
    setlength(pq.a, 1);
    x.b := n;
    x.t := inf;
    f[x.b] := x.t;
    pqins(1 + pq.n, x);

    while pq.n > 0 do begin
        x := pq.a[1];
        pqdel(1);

        i := adj[x.b];
        while i > 0 do begin
            if x.t >= l[i] + c[i] then begin
                y.b := tar[i];
                y.t := l[i] + min(k[i] - 1, (x.t - l[i] - c[i]) div d[i]) * d[i];
                if f[y.b] < y.t then begin
                    f[y.b] := y.t;
                    pqins(1 + pq.n, y);
                end;
            end;
            i := sib[i];
        end;
    end;

    for b := 1 to n-1 do
        if f[b] > 0 then
            writeln(f[b])
        else
            writeln('Unreachable');
end.

```
