program D_Maximize_the_Root;
uses
    math;
const
    nn = 200 * 1000;
    inf = 1000 * 1000 * 1000 + 1;
var
    ntc, tci: int16;
    n, u, v: int32;
    a, mn, adj, par: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;

procedure addarrow(u, v, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure dfs(u: int32);
var
    i, v, h: int32;
begin
    mn[u] := inf;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            dfs(v);
            mn[u] := min(mn[u], mn[v]);
        end;
        i := sib[i];
    end;

    if (mn[u] < inf) and (a[u] < mn[u]) then begin
        h := (mn[u] - a[u]) div 2;
        dec(mn[u], h);
        inc(a[u], h);
    end;

    if u > 1 then mn[u] := min(mn[u], a[u]);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for v := 1 to n do begin
            read(a[v]);
            adj[v] := 0;
        end;
        readln;

        for v := 2 to n do begin
            read(u);
            addarrow(u, v, v);
            addarrow(v, u, -v);
        end;
        readln;

        par[1] := 1;
        dfs(1);
        writeln(a[1] + mn[1]);

    end;
end.
