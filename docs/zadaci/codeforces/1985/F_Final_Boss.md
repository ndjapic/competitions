# Задатак: F_Final_Boss.pas

```pascal
program F_Final_Boss;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    h, n, i: int32;
    t: int64;
    a, c: array [1 .. nn] of int32;
    x: array [1 .. nn] of int64;
    pq: record
        a: array of int32;
        n: int32;
    end;

function prior(l, r: int32): boolean;
begin
    prior := x[l] < x[r];
end;

procedure pqins(v, x: int32);
(* Usage: pqins(pq.n+1, x); *)
var
    u: int32;
begin
    if v = length(pq.a) then setlength(pq.a, 2*v);
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

        readln(h, n);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(c[i]); readln;

        pq.n := 0;
        setlength(pq.a, 1);
        for i := 1 to n do begin
            x[i] := 1;
            pqins(1+pq.n, i);
        end;

        while h > 0 do begin
            i := pq.a[1];
            pqdel(1);
            dec(h, a[i]);
            t := x[i];
            inc(x[i], c[i]);
            pqins(1+pq.n, i);
        end;

        writeln(t)

    end;
end.

```
