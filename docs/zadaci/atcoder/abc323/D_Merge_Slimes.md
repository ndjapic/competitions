# Задатак: D_Merge_Slimes.pas

```pascal
program D_Merge_Slimes;
uses
	math;
const
    maxn = 10 * 1000 * 1000;
type
	telm = record
		s, c: int64;
	end;
var
    n, i: int32;
    ans: int64;
    x: telm;
    pq: record
        a: array [1 .. maxn] of telm;
        n: int32;
    end;

function prior(x, y: telm): boolean;
begin
    prior := x.s < y.s;
end;

procedure pqins(v: int32; x: telm);
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
    pq.n := 0;
	readln(n);

	for i := 1 to n do begin
		read(x.s, x.c);
		pqins(1 + pq.n, x);
	end;

	ans := 0;
	while pq.n > 0 do begin

        x := pq.a[1];
        pqdel(1);

        while (pq.n > 0) and (pq.a[1].s = x.s) do begin
			inc(x.c, pq.a[1].c);
			pqdel(1);
        end;

		inc(ans, x.c mod 2);
		x.s := x.s * 2;
		x.c := x.c div 2;
		if x.c > 0 then pqins(1 + pq.n, x);

	end;

	writeln(ans);
end.

```
