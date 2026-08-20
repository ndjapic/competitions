program C_Heavy_Intervals;
const
    maxn = 100 * 1000;
type
    tarr32 = array [1 .. maxn] of int32;
var
    ntc, tci: int16;
    n, i, j: int32;
    w: int64;
    l, r, c, d: tarr32;
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
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        pq.n := 0;

        for i := 1 to n do begin
            read(l[i]);
            pqins(pq.n+1, l[i]);
        end;
        readln;

        for i := 1 to n do begin
            l[i] := pq.a[1];
            pqdel(1);
        end;

        for i := 1 to n do begin
            read(r[i]);
            pqins(pq.n+1, r[i]);
        end;
        readln;

        for i := 1 to n do begin
            r[i] := pq.a[1];
            pqdel(1);
        end;

        for i := 1 to n do begin
            read(c[i]);
            pqins(pq.n+1, c[i]);
        end;
        readln;

        for i := 1 to n do begin
            c[i] := pq.a[1];
            pqdel(1);
        end;

        j := n;
        for i := n downto 1 do begin
            while (j > 0) and (l[i] < r[j]) do begin
                pqins(pq.n+1, r[j]);
                dec(j);
            end;
            d[i] := pq.a[1] - l[i];
            pqdel(1);
        end;

        for i := 1 to n do pqins(pq.n+1, d[i]);

        for i := 1 to n do begin
            d[i] := pq.a[1];
            pqdel(1);
        end;

        w := 0;
        for i := 1 to n do inc(w, int64(c[i]) * d[n+1-i]);
        writeln(w);

    end;
end.
