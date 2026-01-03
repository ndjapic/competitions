program C_Tree_Cutting;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, k, v, l, r, x, have: int32;
    adj, par, sz: array [1 .. maxn] of int32;
    sib, tar: array [-maxn .. maxn] of int32;

procedure addarrow(u, v, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure readedges(n, m: int32);
var
    u, v, i: int32;
begin
    for v := 1 to n do adj[v] := 0;

    for i := 1 to m do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;

procedure dfs(u: int32);
var
    i, v: int32;
begin
    sz[u] := 1;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            dfs(v);
            if sz[v] >= x then
                inc(have)
            else
                inc(sz[u], sz[v]);
        end;
        i := sib[i];
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        readedges(n, n-1);

        v := 1;
        while (v < n) and (sib[adj[v]] = 0) do inc(v);

        l := 1;
        r := n+1;
        while r-l > 1 do begin

            x := (l+r) div 2;
            par[v] := v;
            have := 0;
            dfs(v);

            if (have > k) or (have = k) and (sz[v] >= x) then
                l := x
            else
                r := x;

        end;

        writeln(l);

    end;
end.
