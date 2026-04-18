program D_Erase_Leaves;
uses
    math;
const
    maxn = 300 * 1000;
var
    n, i, u, v, s, m: int32;
    adj, par, w: array [1 .. maxn] of int32;
    sib, tar: array [-maxn .. maxn] of int32;

procedure addarrow(u, v, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure dfs(u: int32);
var
    i, v: int32;
begin
    w[u] := 1;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            dfs(v);
            inc(w[u], w[v]);
        end;
        i := sib[i];
    end;
end;

begin
    readln(n);

    for v := 1 to n do adj[v] := 0;

    for i := 1 to n-1 do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;

    par[1] := 1;
    dfs(1);

    m := 0;
    s := 1;
    i := adj[1];

    while i <> 0 do begin
        v := tar[i];
        inc(s, w[v]);
        m := max(m, w[v]);
        i := sib[i];
    end;

    writeln(s-m);
end.
