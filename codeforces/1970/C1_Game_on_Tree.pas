program C1_Game_on_Tree;
{$MODE DELPHI}
const
    nn = 200 * 1000;
var
    n, t, u: int32;
    adj, par: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;
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

procedure dfs1(u: int32);
var
    v, i: int32;
begin
    ron[u] := false;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            dfs1(v);
            ron[u] := ron[u] or not ron[v];
        end;
        i := sib[i];
    end;
end;

procedure dfs2(u: int32);
var
    v, i: int32;
begin
    ron[u] := false;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            ron[v] := ron[v] or not ron[u];
            dfs2(v);
        end;
        i := sib[i];
    end;
end;

begin
    readln(n, t);
    readedges(n, n-1);
    readln(u);
    par[1] := 1;
    dfs1(1);
    dfs2(1);
    if ron[u] then
        writeln('Ron')
    else
        writeln('Hermione');
end.
