program D_Rolling_Hash;
uses
    math;
const
    maxn = 16;
var
    p, b: int32;
    n, l, r, lo, hi, mi: int8;
    m, i: int16;
    adj: array [1 .. maxn, 1 .. maxn] of boolean;
    color: array [1 .. maxn+1] of int8;
    seen: array [1 .. maxn, 0 .. maxn] of boolean;

function dfs(k, l: int8): boolean;
var
    c, r: int8;
begin
    if l = 0 then
        dfs := true
    else begin
        for c := 0 to k-1 do seen[l, c] := false;
        for r := l to n do
            if adj[l, r] then seen[l, color[r+1]] := true;
        color[l] := 0;
        while (color[l] < k) and (seen[l, color[r+1]] or not dfs(k, l-1)) do inc(color[l]);
        dfs := color[l] < k;
    end;
end;

begin
    readln(p, b, n, m);

    for l := 1 to n do
        for r := l to n do adj[l, r] := false;

    for i := 1 to m do begin
        readln(l, r);
        adj[l, r] := true;
    end;

    lo := 0;
    hi := n+1;
    color[n+1] := 0;
    if p < n+1 then
        while hi - lo > 1 do begin
            mi := (lo + hi) div 2;
            if dfs(mi, n) then
                hi := mi
            else
                lo := mi;
        end;

    if hi <= p then
        writeln('Yes')
    else
        writeln('No');
end.
