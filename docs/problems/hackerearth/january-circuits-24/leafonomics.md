# Problem: leafonomics.pas

```pascal
program leafonomics;
uses
    math;
const
    maxn = 1000 * 1000 + 1;
    maxw = 1000 * 1000 * 1000;
    maxd = int64(maxn) * maxw;
var
    ntc, tci, n, v: int32;
    adj, par, w, chi: array [1 .. maxn] of int32;
    sib, tar: array [-maxn .. maxn] of int32;
    d1, d2, pre, suf: array [1 .. maxn] of int64;
 
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
        readln(u, v, w[i]);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;
 
procedure dfs1(u: int32);
var
    i, v: int32;
begin
    d1[u] := maxd;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            dfs1(v);
            d1[u] := min(d1[u], w[abs(i)] + d1[v]);
        end;
        i := sib[i];
    end;
    if d1[u] = maxd then d1[u] := 0;
end;
 
procedure dfs2(u: int32);
var
    i, v, deg, c: int32;
begin
    deg := 0;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            inc(deg);
            chi[deg] := i;
        end;
        i := sib[i];
    end;
 
    pre[1] := maxd;
    for c := 1 to deg-1 do begin
        i := chi[c];
        v := tar[i];
        pre[c+1] := min(pre[c], w[abs(i)] + d1[v]);
    end;
 
    suf[deg] := maxd;
    for c := deg downto 2 do begin
        i := chi[c];
        v := tar[i];
        suf[c-1] := min(suf[c], w[abs(i)] + d1[v]);
    end;
 
    for c := 1 to deg do begin
        i := chi[c];
        v := tar[i];
        d2[v] := w[abs(i)] + min(min(pre[c], suf[c]), d2[u]);
    end;
 
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if (par[u] <> v) and (sib[adj[v]] <> 0) then begin
            dfs2(v);
        end;
        i := sib[i];
    end;
end;
 
begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);
        readedges(n, n-1);
 
        par[1] := 1;
        d2[1] := maxd;
        dfs1(1);
        dfs2(1);
 
        for v := 1 to n-1 do write(min(d1[v], d2[v]), ' ');
        writeln(min(d1[n], d2[n]));
    end;
end.

```
