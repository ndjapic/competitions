# Problem: Problem_A2_Ready_Go_Part_2.pas

```pascal
program Problem_A2_Ready_Go_Part_2;
uses
    math;
const
    maxr = 3000;
    maxrc = maxr * maxr;
var
    ntc, tci, r, c, i, j, u, v, w, ans: int32;
    e: int8;
    found: boolean;
    go: array [1 .. maxrc] of char;
    adj: array [1 .. maxrc, 1 .. 4] of int32;
    dsu, sz, component, link, capture, s: array [1 .. maxrc] of int32;

function ij(i, j: int32): int32;
begin
    ij := (i-1) * c + j;
end;

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union_helper(u, v: int32);
begin
    dsu[v] := u;
    inc(sz[u], sz[v]);
end;

procedure union(u, v: int32);
begin
    u := find(u);
    v := find(v);
    if u = v then
    else if sz[u] > sz[v] then
        union_helper(u, v)
    else
        union_helper(v, u);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(r, c);

        u := 0;
        for i := 1 to r do begin
            for j := 1 to c do begin
                inc(u);
                for e := 1 to 4 do adj[u, e] := 0;
                read(go[u]);

                dsu[u] := u;
                sz[u] := 1;
                component[u] := 0;
                capture[u] := 0;
                s[u] := 0;

                if j < c then adj[u, 1] := u+1;
                if i < r then adj[u, 4] := u+c;

                if i > 1 then begin
                    adj[u, 2] := u-c;
                    if (go[u] = 'W') and (go[u-c] = 'W') then
                        union(u, u-c);
                end;

                if j > 1 then begin
                    adj[u, 3] := u-1;
                    if (go[u] = 'W') and (go[u-1] = 'W') then
                        union(u, u-1);
                end;
            end;
            readln;
        end;

        v := 0;
        for i := 1 to r do
            for j := 1 to c do begin
                inc(v);
                if go[v] = 'W' then begin
                    u := find(v);
                    link[v] := component[u];
                    component[u] := v;
                end;
            end;

        ans := 0;
        u := 0;
        for i := 1 to r do
            for j := 1 to c do begin

                inc(u);
                if (go[u] = 'W') and (find(u) = u) then begin
                    v := component[u];
                    found := false;

                    while not found and (v > 0) do begin
                        for e := 1 to 4 do
                            if not found then begin

                                w := adj[v, e];
                                if (w > 0) and (go[w] = '.') then begin
                                    if capture[u] = 0 then
                                        capture[u] := w
                                    else if capture[u] <> w then begin
                                        found := true;
                                        capture[u] := 0;
                                    end;
                                end;

                            end;
                        v := link[v];
                    end;

                    if not found then begin
                        w := capture[u];
                        inc(s[w], sz[u]);
                        ans := max(ans, s[w]);
                    end;
                end;

            end;

        writeln('Case #', tci, ': ', ans);

    end;
end.

```
