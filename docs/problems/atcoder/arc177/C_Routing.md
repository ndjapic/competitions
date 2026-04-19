# Problem: C_Routing.pas

```pascal
program C_Routing;
{$mode delphi}
const
    nn = 500;
    pqnn = nn * nn * 16;
type
    tdata = record
        i, j, d: int16;
    end;
var
    n, i, j, d, ans1, ans2: int16;
    x: tdata;
    c: array [1 .. nn] of string;
    distance: array [1 .. nn, 1 .. nn] of int16;
    pq: record
        a: array [1 .. pqnn] of tdata;
        n: int32;
    end;

function ij(i, j: int16): int32;
begin
    result := int32(i-1) * n + j;
end;

function prior(x, y: tdata): boolean;
begin
    prior := x.d < y.d;
end;

procedure pqins(v: int32; x: tdata);
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

procedure enqueque(i, j, d: int16; ch: char);
var
    x: tdata;
begin
    if c[i][j] <> ch then inc(d);
    if distance[i, j] > d then begin
        distance[i, j] := d;
        x.i := i;
        x.j := j;
        x.d := d;
        pqins(pq.n+1, x);
    end;
end;

begin
    readln(n);
    for i := 1 to n do readln(c[i]);

    (* BEGIN MAJOR DIAGONAL *)

    for i := 1 to n do
        for j := 1 to n do distance[i, j] := 2*n;

    pq.n := 0;
    enqueque(1, 1, 0, 'R');

    while pq.n > 0 do begin

        x := pq.a[1];
        i := x.i;
        j := x.j;
        d := x.d;
        pqdel(1);

        if i > 1 then enqueque(i-1, j, d, 'R');
        if i < n then enqueque(i+1, j, d, 'R');
        if j > 1 then enqueque(i, j-1, d, 'R');
        if j < n then enqueque(i, j+1, d, 'R');

    end;

    ans1 := distance[n, n];

    (* END MAJOR DIAGONAL *)

    (* BEGIN MINOR DIAGONAL *)

    for i := 1 to n do
        for j := 1 to n do distance[i, j] := 2*n;

    pq.n := 0;
    enqueque(1, n, 0, 'B');

    while pq.n > 0 do begin

        x := pq.a[1];
        i := x.i;
        j := x.j;
        d := x.d;
        pqdel(1);

        if i > 1 then enqueque(i-1, j, d, 'B');
        if i < n then enqueque(i+1, j, d, 'B');
        if j > 1 then enqueque(i, j-1, d, 'B');
        if j < n then enqueque(i, j+1, d, 'B');

    end;

    ans2 := distance[n, 1];

    (* END MINOR DIAGONAL *)

    writeln(ans1 + ans2);
end.

```
