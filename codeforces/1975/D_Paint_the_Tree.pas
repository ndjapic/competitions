program D_Paint_the_Tree;
{$MODE DELPHI}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, a, b, c, d, ans: int32;
    adj, par: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;

procedure addarrow(x, y, i: int32);
begin
    sib[i] := adj[x];
    adj[x] := i;
    tar[i] := y;
end;

procedure readedges(n: int32);
var
    x, y, i: int32;
begin
    for x := 1 to n do adj[x] := 0;

    for i := 1 to n-1 do begin
        readln(x, y);
        addarrow(x, y, i);
        addarrow(y, x, -i);
    end;
end;

function dfs(u: int32): int32;
var
    i, v: int32;
begin
    result := 0;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            result := max(result, dfs(v) + 1);
        end;
        i := sib[i];
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(a, b);
        readedges(n);

        par[a] := a;
        d := dfs(a);

        c := b;
        d := 0;
        while c <> a do begin
            c := par[c];
            inc(d);
        end;

        ans := (d+1) div 2;
        c := b;
        d := ans;
        while d > 0 do begin
            c := par[c];
            dec(d);
        end;
        
        par[c] := c;
        inc(ans, 2*n-2-dfs(c));
        writeln(ans);

    end;
end.
