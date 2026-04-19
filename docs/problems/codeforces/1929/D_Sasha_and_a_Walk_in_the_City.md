# Problem: D_Sasha_and_a_Walk_in_the_City.pas

```pascal
program D_Sasha_and_a_Walk_in_the_City;
const
    maxn = 300 * 1000;
    lg2maxn = 18;
    prime = 998244353;
var
    ntc, tci: int16;
    n, m, i, u, v: int32;
    adj, par, w: array [1 .. maxn] of int32;
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
    readln(n); (* m := n-1; (tree) *)
    readedges(n, n-1);

    par[1] := 1;
    dfs(1);
end.

```
