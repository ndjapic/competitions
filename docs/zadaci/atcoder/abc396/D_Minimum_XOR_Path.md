# Задатак: D_Minimum_XOR_Path.pas

```pascal
program D_Minimum_XOR_Path;
uses
    math;
const
    nn = 10;
    mm = 45;

var
    n, m: int8;
    ans: int64;
    adj: array [1 .. nn] of int8;
    visited: array [1 .. nn] of boolean;
    sib, tar: array [-mm .. mm] of int8;
    w: array [1 .. mm] of int64;

procedure addarrow(u, v, i: int8);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure readedges(n, m: int8);
var
    u, v, i: int8;
begin
    for v := 1 to n do begin
        adj[v] := 0;
        visited[v] := false;
    end;

    for i := 1 to m do begin
        readln(u, v, w[i]);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;

procedure dfs(u: int8; x: int64);
var
    i, v: int8;
begin
    visited[u] := true;
    if u = n then
        ans := min(ans, x)
    else begin
        i := adj[u];
        while i <> 0 do begin
            v := tar[i];
            if not visited[v] then
                dfs(v, x xor w[abs(i)]);
            i := sib[i];
        end;
    end;
    visited[u] := false;
end;

begin
    readln(n, m);
    readedges(n, m);
    ans := int64(1) shl 60;
    dfs(1, 0);
    writeln(ans);
end.

```
