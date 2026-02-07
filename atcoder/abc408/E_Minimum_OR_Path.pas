program E_Minimum_OR_Path;
uses
    math;
const
    nn = 200 * 1000;
var
    n, m, ans: int32;
    adj, vw: array [1 .. nn] of int32;
    sib, tar, ew: array [-nn .. nn] of int32;
    visited: array [1 .. nn] of boolean;

procedure addarrow(u, v, w, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
    ew[i] := w;
end;

procedure readedges(n, m: int32);
var
    u, v, w, i: int32;
begin
    for v := 1 to n do begin
        adj[v] := 0;
        visited[v] := false;
    end;

    for i := 1 to m do begin
        readln(u, v, w);
        addarrow(u, v, w, i);
        addarrow(v, u, w, -i);
    end;
end;

procedure dfs(u: int32);
var
    i, v: int32;
begin
    visited[u] := true;
    if u = n then
        ans := min(ans, vw[u])
    else begin

        i := adj[u];
        while i <> 0 do begin
            v := tar[i];
            if not visited[v] then begin
                vw[v] := vw[u] or ew[i];
                dfs(v);
            end;
            i := sib[i];
        end;

    end;
    visited[u] := false;
end;

begin
    readln(n, m);
    readedges(n, m);

    ans := 1 shl 30;
    vw[1] := 0;
    dfs(1);
    writeln(ans);
end.
