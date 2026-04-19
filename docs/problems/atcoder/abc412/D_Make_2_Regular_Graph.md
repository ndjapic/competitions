# Problem: D_Make_2_Regular_Graph.pas

```pascal
program D_Make_2_Regular_Graph;
const
    nn = 8;
    maxq = 1024 * 1024;
var
    n, m, i, a, b: int8;
    adj, p2: qword;
    pq: record
        a: array [1 .. maxq] of begin
            adj: qword;
            d: int8;
        end;
        n: int32;
    end;

function goal(adj: qword): int8;
var
    d, c1, e: int8;
begin
    d := 0;
    while adj > 0 do begin
        c1 := 0;
        for e := 0 to n-1 do begin
            inc(c1, adj mod 2);
            adj := adj div 2;
        end;
        inc(d, abs(c1-2));
    end;
    goal := d;
end;

procedure toggle(a, b: int8);
begin
    adj := adj xor (qword(1) shl ((a-1) * n + b-1));
end;

function pqlt(x, y: int32): boolean;
begin
    pqlt := x < y;
end;

procedure pqins(v, x: int32);
(* Usage: pqins(pq.n+1, x); *)
var
    u: int32;
begin
    u := v div 2;
    if (v > 1) and pqlt(x, pq.a[u]) then begin
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
    if (v+1 <= pq.n-1) and pqlt(pq.a[v+1], pq.a[v]) then inc(v);
    if (v <= pq.n-1) and pqlt(pq.a[v], pq.a[pq.n]) then begin
        pq.a[u] := pq.a[v];
        pqdel(v);
    end else begin
        pq.a[u] := pq.a[pq.n];
        dec(pq.n);
    end;
end;

begin
    pq.n := 0;
    readln(n, m);

    adj := 0;
    p2 := qword(1) shl n;

    for i := 1 to m do begin
        readln(a, b);
        toggle(a, b);
        toggle(a, b);
    end;

end.

```
