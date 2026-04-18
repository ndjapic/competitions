# Задатак: rebelreach.pas

```pascal
program rebelreach;
const
    maxn = 1000 * 1000;
    maxe = 19;
var
    ntc, tci, n, q, k, u, v: int32;
    x: int64;
    e: int8;
    adj: array [1 .. maxn] of int32;
    sib, tar: array [-maxn .. maxn] of int32;
    anc: array [1 .. maxn] of array [0 .. maxe] of int32;
    guards: array [1 .. maxn] of array [0 .. maxe] of int64;
 
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
    i, v: int32;
    e: int8;
begin
    for e := 0 to maxe-1 do begin
        anc[u][e+1] := anc[anc[u][e]][e];
        guards[u][e+1] := guards[u][e] + guards[anc[u][e]][e];
    end;
 
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if anc[u][0] <> v then begin
            anc[v][0] := u;
            dfs(v);
        end;
        i := sib[i];
    end;
end;
 
begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);
        readedges(n, n-1);
 
        for v := 1 to n do read(guards[v][0]);
        readln;
 
        anc[1][0] := 1;
        dfs(1);
 
        readln(q);
        for k := 1 to q do begin
 
            readln(u, x);
 
            for e := maxe downto 0 do
                if guards[u][e] < x then begin
                    dec(x, guards[u][e]);
                    u := anc[u][e];
                end;
 
            writeln(u);
 
        end;
    end;
end.

```
