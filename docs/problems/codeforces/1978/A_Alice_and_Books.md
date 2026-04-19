# Problem: A_Alice_and_Books.pas

```pascal
program A_Alice_and_Books;
{$mode objfpc}{$H+}{$J-}
const
    nn = 100;
var
    ntc, tci: int16;
    n, i: int8;
    x: int32;
var
    pq: record
        a: array [1 .. nn] of int32;
        n: int32;
    end;

function prior(l, r: int32): boolean;
begin
    prior := l > r;
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

        for i := 1 to n-1 do begin
            read(x);
            pqins(1 + pq.n, x);
        end;

        readln(x);
        writeln(x + pq.a[1]);

    end;
end.

```
