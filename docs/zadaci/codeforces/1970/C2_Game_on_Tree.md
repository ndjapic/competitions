# Задатак: C2_Game_on_Tree.pas

```pascal
program C2_Game_on_Tree;
{$MODE DELPHI}
const
    nn = 200 * 1000;
var
    n, t, u, i: int32;
    adj, par, deg: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;
    chi: array [1 .. nn] of array of int32;
    pre, suf: array [1 .. nn] of array of boolean;
    ron: array [1 .. nn] of boolean;

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
    v, i: int32;
begin
    {writeln('Entering dfs ', u);}
    ron[u] := false;
    deg[u] := 0;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            if deg[u] = length(chi[u]) then setlength(chi[u], 2*deg[u]+1);
            chi[u][deg[u]] := v;
            inc(deg[u]);
            dfs(v);
        end;
        i := sib[i];
    end;

    setlength(chi[u], deg[u]);
    setlength(pre[u], deg[u]+1);
    setlength(suf[u], deg[u]+1);

    pre[u][0] := false;
    suf[u][0] := false;
    for i := 1 to deg[u] do begin
        v := chi[u][i-1];
        pre[u][i] := pre[u][i-1] or not pre[v][deg[v]];
        v := chi[u][deg[u]-i];
        suf[u][i] := suf[u][i-1] or not pre[v][deg[v]];
    end;
    ron[u] := ron[u] or pre[u][deg[u]];

    for i := 0 to deg[u]-1 do begin
        v := chi[u][i];
        ron[v] := ron[v] or not pre[u][i] or not suf[u][deg[u]-1-i];
    end;
    {writeln('Exiting dfs ', u);}
end;

begin
    readln(n, t);
    readedges(n, n-1);
    par[1] := 1;
    dfs(1);

    for i := 1 to t do begin
        read(u);
        if ron[u] then
            writeln('Ron')
        else
            writeln('Hermione');
    end;
    readln;
end.

```
