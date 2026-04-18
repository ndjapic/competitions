program D_XOR_Shortest_Walk;
const
    nn = 1024;
var
    n, m, e: int32;
    adj, x: array [1 .. nn] of int32;
    sib, tar, w: array [1 .. nn] of int32;
    seen: array [1 .. nn] of int8;

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
        x[v] := 0;
        seen[v] := 0;
    end;

    for i := 1 to m do begin
        readln(u, v, w[i]);
        addarrow(u, v, i);
    end;
end;

procedure dfs(u: int32);
var
    i, v, e, f: int32;
    found: boolean;
begin
if not seen[u] then begin
    seen[u] := true;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        found := false;

        for e := 0 to nn-1 do
            if odd(x[u] shr e) then begin
                f := e xor w[i];
                found := found or not odd(x[v] shr f);
                x[v] := x[v] or (int32(1) shl f);
            end;

        {if found then} dfs(v);
        i := sib[i];
    end;
    seen[u] := false;
end;
end;

begin
    readln(n, m);
    readedges(n, m);

    x[1] := 1;
    dfs(1);

    e := 0;
    while (e < nn) and not odd(x[n] shr e) do inc(e);

    if e = nn then
        writeln(-1)
    else
        writeln(e);
end.
