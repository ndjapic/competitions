# Задатак: F_Microcycle.pas

```pascal
program F_Microcycle;
uses
    math;
const
    maxn = 200 * 1000;
    lg2maxn = 17;
    inf = 1000 * 1000 + 1;
var
    ntc, tci: int16;
    n, m, i, u, v, time, ku, kv: int32;
    ans: record
        u, v, b: int32;
    end;
    adj, time1, time2, w, u2lca, v2lca: array [1 .. maxn] of int32;
    sib, tar: array [-maxn .. maxn] of int32;
    anc, mn: array [1 .. maxn] of array [0 .. lg2maxn] of int32;
    seen: array [1 .. maxn] of boolean;

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

function is_anc(u, v: int32): boolean;
begin
    is_anc := (time1[u] <= time1[v]) and (time2[v] <= time2[u]);
end;
{
function lca(u, v: int32): int32;
var
    e: int8;
begin
    if not is_anc(u, v) then begin
        for e := lg2maxn downto 0 do
            if not is_anc(anc[u][e], v) then
                u := anc[u][e];
        u := anc[u][0];
    end;
    lca := u;
end;
}
function get_mn(u, v: int32): int32;
var
    ans: int32;
    e: int8;
begin
    ans := inf;
    if not is_anc(u, v) then begin
        for e := lg2maxn downto 0 do
            if not is_anc(anc[u][e], v) then begin
                ans := min(ans, mn[u][e]);
                u := anc[u][e];
            end;
        ans := min(ans, mn[u][0]);
        u := anc[u][0];
    end;
    get_mn := ans;
end;

procedure dfs1(u: int32);
var
    i, v: int32;
    e: int8;
begin
    inc(time);
    time1[u] := time;

    seen[u] := true;
    for e := 0 to lg2maxn-1 do begin
        v := anc[u][e];
        anc[u][e+1] := anc[v][e];
        mn[u][e+1] := min(mn[u][e], mn[v][e]);
    end;

    i := adj[u];
    while i <> 0 do begin
        v := tar[i];

        if anc[u][0] = v then
            (* pass *)
        else if not seen[v] then begin

            anc[v][0] := u;
            mn[v][0] := w[abs(i)];
            dfs1(v);

        end;

        i := sib[i];
    end;

    inc(time);
    time2[u] := time;
end;

procedure dfs2(u: int32);
var
    i, v, b: int32;
begin
    seen[u] := true;
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];

        if anc[u][0] = v then
            (* pass *)
        else if seen[v] then begin

            b := min(
                w[abs(i)],
                min(
                    get_mn(u, v),
                    get_mn(v, u)
                )
            );

            if b < ans.b then begin
                ans.b := b;
                ans.u := u;
                ans.v := v;
            end;

        end else
            dfs2(v);

        i := sib[i];
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        readedges(n, m);

        for v := 1 to n do seen[v] := false;

        time := 0;
        for v := 1 to n do
            if not seen[v] then begin
                mn[v][0] := inf;
                anc[v][0] := v;
                dfs1(v);
            end;

        for v := 1 to n do seen[v] := false;

        ans.b := inf;
        for v := 1 to n do
            if not seen[v] then dfs2(v);

        u := ans.u;
        v := ans.v;
        ku := 0;
        kv := 0;

        while not is_anc(u, v) do begin
            inc(ku);
            u2lca[ku] := u;
            u := anc[u][0];
        end;

        while not is_anc(v, u) do begin
            inc(kv);
            v2lca[kv] := v;
            v := anc[v][0];
        end;

        writeln(ans.b, ' ', ku+kv+1);
        for i := 1 to ku do write(u2lca[i], ' ');
        write(u);
        for i := kv downto 1 do write(' ', v2lca[i]);
        writeln;

    end;
end.

```
