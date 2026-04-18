# Задатак: E_Non_Decreasing_Colorful_Path.pas

```pascal
program E_Non_Decreasing_Colorful_Path;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, m, s, t, ns: int32;
    a, score, dsu, size, adj, urr, vrr, s, merge: array [1 .. maxn] of int32;
    sib, tar: array [1 .. maxn] of int32;

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union2(u, v: int32);
begin
    dsu[v] := u;
    inc(size[u], size[v]);
end;

procedure union1(u, v: int32);
begin
    u := find(u);
    v := find(v);
    if u = v then
    else if size[u] > size[v] then
        union2(u, v)
    else
        union2(v, u);
end;

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
        read(a[v]);
        dsu[v] := v;
        size[v] := 1;
    end;
    readln;

    for i := 1 to m do begin
        readln(u, v);
        urr[i] := u;
        vrr[i] := v;
        if a[u] = a[v] then union1(u, v);
    end;

    for i := 1 to m do begin
        u := find(urr[i]);
        v := find(vrr[i]);
        if a[u] < a[v] then
            addarrow(u, v, i)
        else if a[v] < a[u] then
            addarrow(v, u, i);
    end;
end;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (s[j] <= s[k]) then begin
                merge[i] := s[j];
                inc(j);
            end else begin
                merge[i] := s[k];
                inc(k);
            end;

        for i := l to r-1 do s[i] := merge[i];

    end;
end;

procedure dfs(u: int32);
var
    i, v: int32;
begin
    if u <> t then begin

        ns := 0;
        i := adj[u];
        while i <> 0 do begin
            v := tar[i];
            inc(ns);
            s[ns] := v;
            i := sib[i];
        end;

        msort(1, ns);

        score[u] := 0;
        for i := 1 to ns do
            if 
        i := adj[u];
        while i <> 0 do begin
            v := tar[i];
            dfs(v);
            if score[v] > 0 then score[u] := max(score[u], score[v]+1);
            i := sib[i];
        end;

    end;
end;

begin
    readln(n, m);
    readedges(n, m);

    s := find(1);
    t := find(n);
    score[t] := 1;
    dfs(s);
    writeln(score[s]);
end.

```
