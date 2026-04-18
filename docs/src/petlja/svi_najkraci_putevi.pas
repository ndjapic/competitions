program svi_najkraci_putevi;
uses
    math;
const
    maxn = 750;
    maxm = 4 * maxn;
    maxw = 1000;
    inf = maxm * maxw;
type
    pqelm = record
        v: int16;
        d: int32;
    end;
var
    n, m, i, u, v: int16;
    ans: int32;
    x, y: pqelm;
    adj: array [0 .. maxn] of int16;
    sib, tar: array [-maxm .. maxm] of int16;
    w: array [-maxm .. maxm] of int32;
    d: array [0 .. maxn] of int32;
    pq: record
        a: array of pqelm;
        n: int32;
    end;

procedure addarrow(u, v, i: int16);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure readedges(n, m: int16);
var
    u, v, i: int16;
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
    prior := x.d < y.d;
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
    readln(n);
    readln(m);
    readedges(n, m);

    ans := 0;
    setlength(pq.a, 1);
    for u := 0 to n-1 do begin

        pq.n := 0;
        for v := 0 to n-1 do d[v] := inf;
        d[u] := 0;
        x.v := u;
        x.d := 0;
        pqins(pq.n+1, x);

        while pq.n > 0 do begin

            x := pq.a[1];
            pqdel(1);

            i := adj[x.v];
            while i <> 0 do begin
                y.v := tar[i];
                y.d := x.d + w[i];
                if d[y.v] > y.d then begin
                    d[y.v] := y.d;
                    pqins(pq.n+1, y);
                end;
                i := sib[i];
            end;

        end;

        i := adj[u];
        while i <> 0 do begin
            v := tar[i];
            ans := max(ans, w[i] - d[v]);
            i := sib[i];
        end;

    end;

    writeln(ans);
end.
