# Problem: G_Bicycles.pas

```pascal
program G_Bicycles;
uses
    math;
const
    maxn = 1000;
    maxw = 100 * 1000;
    maxq = 1 * maxn;
    inf = maxw * maxn * maxn + 1;
    {inf = high(int32);}
type
    tarr16 = array [1 .. maxn] of int16;
    pqelm = int16;
var
	ntc, tci: int8;
    n, m, i, l, r, u0, u, v: int16;
    s, p, merge: tarr16;
    w, t: array [1 .. maxn] of int64;
    adj: array [1 .. maxn] of int16;
    sib, tar: array [-maxn .. maxn] of int16;
    d: array [1 .. maxn, 1 .. maxn] of int64;
    pq: record
        a: array [1 .. maxq] of pqelm;
        n: int32;
    end;

procedure msorti(var indices, priority: tarr16; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msorti(indices, priority, l, m);
        msorti(indices, priority, m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                (priority[indices[j]] >= priority[indices[k]]) {and (
                    (priority[indices[j]] > priority[indices[k]]) or
                    (indices[j] <= indices[k])
                )}
            ) then begin
                merge[i] := indices[j];
                inc(j);
            end else begin
                merge[i] := indices[k];
                inc(k);
            end;

        for i := l to r-1 do indices[i] := merge[i];

    end;
end;

function prior(x, y: pqelm): boolean;
begin
    prior := d[u0, x] < d[u0, y];
end;

procedure pqins(v: int32; x: pqelm);
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

procedure addarrow(u, v, i: int16);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, m);

        for v := 1 to n do begin
            adj[v] := 0;
        end;

		for i := 1 to m do begin
			readln(u, v, w[i]);
            addarrow(u, v, i);
            addarrow(v, u, -i);
		end;

        pq.n := 0;

        for u0 := 1 to n do begin

            for u := 1 to n do d[u0, u] := inf;
            d[u0, u0] := 0;
            pqins(pq.n+1, u0);

            while pq.n > 0 do begin

                u := pq.a[1];
                pqdel(1);

                i := adj[u];
                while i <> 0 do begin
                    v := tar[i];
                    if d[u0, v] > d[u0, u] + w[abs(i)] then begin
                        d[u0, v] := d[u0, u] + w[abs(i)];
                        pqins(pq.n+1, v);
                    end;
                    i := sib[i];
                end;

            end;

        end;

        for v := 1 to n do begin
            read(s[v]);
            p[v] := v;
        end;
        readln;
        msorti(p, s, 1, n);

        l := n;
        t[n] := 0;
        repeat
            dec(l);
            t[l] := inf;
            for r := l+1 to n do
                t[l] := min(t[l], d[p[l], p[r]] * s[p[l]] + t[r]);
        until p[l] = 1;

        writeln(t[l]);

    end;
end.

```
