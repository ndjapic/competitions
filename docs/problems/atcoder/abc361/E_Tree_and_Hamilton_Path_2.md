# Problem: E_Tree_and_Hamilton_Path_2.pas

```pascal
program E_Tree_and_Hamilton_Path_2;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    maxn = 200 * 1000;
var
    n, i: int32;
    ans: int64;
    adj, par, c: array [1 .. maxn] of int32;
    d: array [1 .. maxn] of int64;
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
        readln(u, v, c[i]);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;

procedure dfs(u: int32);
var
    i, v: int32;
begin
    d[u] := 0;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            dfs(v);
            ans := max(ans, d[u] + c[abs(i)] + d[v]);
            d[u] := max(d[u], c[abs(i)] + d[v]);
        end;
        i := sib[i];
    end;
end;

begin
    readln(n);
    readedges(n, n-1);

    ans := 0;
    par[1] := 1;
    dfs(1);

    ans := -ans;
    for i := 1 to n-1 do inc(ans, 2*c[i]);
    writeln(ans);
end.

```
