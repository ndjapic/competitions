# Задатак: F_MST_Query.pas

```pascal
program F_MST_Query;
uses
    math;
const
    nn = 200 * 1000;
    lg2nn = 17;
var
    n, q, i, u, v, w, w1, w2, l, time, total, ans: int32;
    e: int8;
    adj, par, time1, time2: array [1 .. nn] of int32;
    sib, tar, c: array [-nn .. nn] of int32;
    anc, mx: array [1 .. nn] of array [0 .. lg2nn] of int32;

procedure addarrow(u, v, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure readedges(n: int32);
var
    u, v, i: int32;
begin
    for v := 1 to n do adj[v] := 0;
    total := 0;

    for i := 1 to n-1 do begin
        readln(u, v, c[i]);
        addarrow(u, v, i);
        addarrow(v, u, -i);
        c[-i] := c[i];
        inc(total, c[i]);
    end;
end;

procedure dfs(u: int32);
var
    i, v: int32;
    e: int8;
begin
    inc(time);
    time1[u] := time;

    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;

            anc[v][0] := u;
            mx[v][0] := c[i];
            for e := 0 to lg2nn-1 do begin
                anc[v][e+1] := anc[anc[v][e]][e];
                mx[v][e+1] := max(mx[v][e], mx[anc[v][e]][e]);
            end;

            dfs(v);
        end;
        i := sib[i];
    end;

    inc(time);
    time2[u] := time;
end;

function is_anc(u, v: int32): boolean;
begin
    is_anc := (time1[u] <= time1[v]) and (time2[v] <= time2[u]);
end;

procedure lca(u, v: int32; var ans, w: int32);
var
    e: int8;
begin
    w := 0;
    if not is_anc(u, v) then begin
        for e := lg2nn downto 0 do
            if not is_anc(anc[u][e], v) then begin
                w := max(w, mx[u][e]);
                u := anc[u][e];
            end;
        w := max(w, mx[u][0]);
        u := anc[u][0];
    end;
    ans := u;
end;

begin
    readln(n, q);
    readedges(n);

    par[1] := 1;
    for e := 0 to lg2nn do begin
        anc[1][e] := 1;
        mx[1][e] := 0;
    end;
    dfs(1);

    for i := 1 to q do begin
        readln(u, v, w);
        ans := total + w;
        lca(u, v, l, w1);
        lca(v, u, l, w2);
        w := max(w, w1);
        w := max(w, w2);
        writeln(ans - w);
    end;
end.

```
