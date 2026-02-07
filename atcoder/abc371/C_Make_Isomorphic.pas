program C_Make_Isomorphic;
uses
    math;
const
    nn = 8;
var
    n, mg, mh, i, u, v: int8;
    p: array [1 .. nn] of int8;
    mincost: int32;
    g, h: array [1 .. nn, 1 .. nn] of boolean;
    a: array [1 .. nn, 1 .. nn] of int32;

procedure dfs(v: int8);
var
    u, w, i, j: int8;
    cost: int32;
begin
    if v > 0 then begin

        for u := 1 to v do begin
            w := p[u];
            p[u] := p[v];
            p[v] := w;

            dfs(v-1);

            w := p[u];
            p[u] := p[v];
            p[v] := w;
        end;

        cost := 0;
        for i := 1 to n-1 do
            for j := i+1 to n do
                if g[i, j] <> h[p[i], p[j]] then
                    inc(cost, a[p[i], p[j]]);
        mincost := min(mincost, cost);

    end;
end;

begin
    readln(n);

    for u := 1 to n do
        for v := 1 to n do begin
            g[u, v] := false;
            h[u, v] := false;
        end;

    readln(mg);
    for i := 1 to mg do begin
        readln(u, v);
        g[u, v] := true;
        g[v, u] := true;
    end;

    readln(mh);
    for i := 1 to mh do begin
        readln(u, v);
        h[u, v] := true;
        h[v, u] := true;
    end;

    for u := 1 to n-1 do begin
        for v := u+1 to n do begin
            read(a[u, v]);
            a[v, u] := a[u, v];
        end;
        readln;
    end;

    mincost := high(int32);
    for v := 1 to n do p[v] := v;
    dfs(n);
    writeln(mincost);
end.
