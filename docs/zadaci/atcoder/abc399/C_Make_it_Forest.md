# Задатак: C_Make_it_Forest.pas

```pascal
program C_Make_it_Forest;
const
    nn = 200 * 1000;
var
    n, m, u, ans: int32;
    adj, par: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;
    active, seen: array [1 .. nn] of boolean;

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
    for v := 1 to n do begin
        adj[v] := 0;
        par[v] := v;
        seen[v] := false;
    end;

    for i := 1 to m do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(v, u, -i);
        active[i] := true;
    end;
end;

procedure dfs(u: int32);
var
    i, v: int32;
begin
    seen[u] := true;
    i := adj[u];
    while i <> 0 do begin
        if active[abs(i)] then begin
            v := tar[i];
            if par[u] <> v then
                if seen[v] then begin
                    active[abs(i)] := false;
                    inc(ans);
                end else begin
                    par[v] := u;
                    dfs(v);
                end;
        end;
        i := sib[i];
    end;
end;

begin
    readln(n, m);
    readedges(n, m);

    ans := 0;
    for u := n downto 1 do
        if not seen[u] then dfs(u);

    writeln(ans);
end.

```
