program D_Doremy_s_Connecting_Plan;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci, n, c, i, j: int32;
    ans: boolean;
    a: array [1 .. maxn] of int64;
    mn: array [1 .. maxn] of int32;
    pq: record
        a: array [1 .. maxn] of int32;
        n: int32;
    end;

function find(v: int32): int32;
begin
    if mn[mn[v]] <> mn[v] then mn[v] := find(mn[v]);
    find := mn[v];
end;

procedure union1(u, v: int32);
begin
    mn[v] := u;
    inc(a[u], a[v]);
end;

function prior(x, y: int32): boolean;
begin
    x := find(x);
    y := find(y);
    prior := a[x] / mn[x] > a[y] / mn[y];
    {prior := x < y;}
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

        readln(n, c);
        pq.n := 0;

        for i := 1 to n do begin
            read(a[i]);
            mn[i] := i;
        end;
        readln;

        ans := true;
        if a[1] = 0 then begin

            for i := 2 to n do pqins(pq.n+1, i);

            while ans and (pq.n > 0) do begin
                i := find(pq.a[1]);
                pqdel(1);
                ans := a[1] + a[i] >= int64(c) * 1 * i;
                if ans then union1(1, i);
            end;

        end else begin

            for i := 1 to n do pqins(pq.n+1, i);

            while ans and (pq.n > 1) do begin
                i := find(pq.a[1]);
                pqdel(1);
                j := find(pq.a[1]);
                pqdel(1);
                ans := a[i] + a[j] >= int64(c) * i * j;
                if ans then union1(min(i, j), max(i, j));
                pqins(pq.n, min(i, j));
            end;

        end;

        if ans then
            writeln('Yes')
        else
            writeln('No');

    end;
end.
