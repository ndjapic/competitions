# Problem: euler107.pas

```pascal
program Minimal_network;
uses
    math;
const
    maxn = 3000;
    maxm = maxn * (maxn - 1) div 2;
type
    pqelm = int32;
var
    n, m, i, v: int32;
    ans: int32;
    adj: array [1 .. maxn] of int32;
    seen: array [1 .. maxn] of boolean;
    sib, tar, w: array [-maxm .. maxm] of int32;
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
    u, v, i: int32;
begin
    for v := 1 to n do adj[v] := 0;

    for i := 1 to m do begin
        readln(u, v, w[i]);
        addarrow(u, v, i);
        addarrow(v, u, -i);
        w[-i] := w[i];
    end;
end;

function prior(x, y: pqelm): boolean;
begin
    prior := w[x] < w[y];
end;

procedure pqins(v: int32; x: pqelm);
(* Usage: pqins(pq.n+1, x); *)
var
    u: int32;
begin
    if length(pq.a) = v then setlength(pq.a, 2*v);
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
    setlength(pq.a, 1);

    readln(n, m);
    readedges(n, m);

    ans := 0;
    for v := 2 to n do seen[v] := false;
    seen[1] := true;
    i := adj[1];
    while i <> 0 do begin
        pqins(1 + pq.n, i);
        i := sib[i];
    end;

    while pq.n > 0 do begin

        i := pq.a[1];
        pqdel(1);
        v := tar[i];

        if not seen[v] then begin
            seen[v] := true;
            inc(ans, w[i]);
            i := adj[v];
            while i <> 0 do begin
                pqins(1 + pq.n, i);
                i := sib[i];
            end;
        end;

    end;

    writeln(ans);
end.

```
