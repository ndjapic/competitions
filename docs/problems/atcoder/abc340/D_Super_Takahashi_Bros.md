# Problem: D_Super_Takahashi_Bros.pas

```pascal
program D_Super_Takahashi_Bros;
const
    maxn = 200 * 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    n, i, j: int32;
    a, b, x: array [1 .. maxn] of int64;
    d: array [1 .. maxn] of int64;
    pq: record
        a: array of int32;
        n: int32;
    end;

function prior(i, j: int32): boolean;
begin
    prior := d[i] < d[j];
end;

procedure pqins(v, i: int32);
(* Usage: pqins(pq.n+1, x); *)
var
    u: int32;
begin
    if v = length(pq.a) then setlength(pq.a, 2*v);
    u := v div 2;
    if (v > 1) and prior(i, pq.a[u]) then begin
        pq.a[v] := pq.a[u];
        pqins(u, i);
    end else begin
        pq.a[v] := i;
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
    pq.n := 0;
    setlength(pq.a, 1);
    readln(n);

    for i := 1 to n-1 do readln(a[i], b[i], x[i]);

    for i := 1 to n do d[i] := inf;
    d[1] := 0;
    pqins(pq.n+1, 1);

    while pq.n > 0 do begin
        i := pq.a[1];
        pqdel(1);

        j := i+1;
        if (i < n) and (d[j] > d[i] + a[i]) then begin
            d[j] := d[i] + a[i];
            pqins(pq.n+1, j);
        end;

        j := x[i];
        if (i < n) and (d[j] > d[i] + b[i]) then begin
            d[j] := d[i] + b[i];
            pqins(pq.n+1, j);
        end;

    end;

    writeln(d[n]);
end.

```
