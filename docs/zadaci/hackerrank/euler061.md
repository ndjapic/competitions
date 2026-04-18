# Задатак: euler061.pas

```pascal
program euler061;
uses
    math;
const
    maxpq = 20 * 1000 * 1000;
    minn = 18;
    maxn = 141;
var
    t, i: int8;
    n: int32;
    s: array [1 .. 6] of int8;
    fig: array [1 .. 6] of int32;
    p: array [1 .. 8, 1 .. maxn] of int32;
    pq: record
        a: array [1 .. maxpq] of int32;
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

function bisect(i: int8; x: int32): int32;
var
    l, r, m: int32;
begin
    l := minn - 1;
    r := maxn;
    while r-l > 1 do begin
        m := (l+r) div 2;
        if p[s[i], m] < x then
            l := m
        else
            r := m;
    end;
    bisect := r;
end;

procedure dfs(i, par, seen: int8; sum: int32);
var
    n, l, r: int32;
    j, p2: int8;
begin
    if i = t then begin
        l := 1000;
        r := 9999;
    end else begin
        l := fig[par] mod 100 * 100 + 0;
        r := l - 0 + 99;
    end;

    n := bisect(i, l);
    fig[i] := p[s[i], n];
    inc(seen, 1 shl (i-1));

    while fig[i] <= r do begin
        {writeln(i, ' ', fig[i]); flush(output);}
        if min(fig[i] mod 100, fig[i] div 100) >= 10 then begin

            if seen + 1 < 1 shl t then begin

                p2 := 1;
                for j := 1 to t-1 do begin
                    if seen and p2 = 0 then
                        dfs(j, i, seen, sum + fig[i]);
                    inc(p2, p2);
                end;

            end else begin

                if fig[t] div 100 = fig[i] mod 100 then
                    pqins(pq.n+1, sum + fig[i]);
                {for j := 1 to t do write(fig[j], ' ');
                writeln; flush(output);}

            end;

        end;
        inc(n);
        fig[i] := p[s[i], n];
    end;
end;

begin
    readln(t);
    for i := 1 to t do read(s[i]);
    readln;

    pq.n := 0;
    for n := minn to maxn do begin
        p[3, n] := n * (n+1) div 2;
        p[4, n] := n * n;
        p[5, n] := n * (3*n-1) div 2;
        p[6, n] := n * (2*n-1);
        p[7, n] := n * (5*n-3) div 2;
        p[8, n] := n * (3*n-2);
    end;

    dfs(t, t, 0, 0);

    while pq.n > 0 do begin

        writeln(pq.a[1]);
        pqdel(1);

        {if pq.n > 0 then
            write(' ');}

    end;
    {writeln;}
end.

```
