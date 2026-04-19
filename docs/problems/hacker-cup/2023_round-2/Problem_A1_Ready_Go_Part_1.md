# Problem: Problem_A1_Ready_Go_Part_1.pas

```pascal
program Problem_A1_Ready_Go_Part_1;
const
    maxr = 21;
    maxrc = maxr * maxr;
var
    ntc, tci, r, c, i, j, u, v: int16;
    e: int8;
    ans: boolean;
    go: array [0 .. maxrc] of char;
    seen: array [0 .. maxrc] of boolean;
    adj: array [0 .. maxrc, 1 .. 4] of int16;

function ij(i, j: int16): int16;
begin
    ij := i * (c+2) + j;
end;

function dfs(u: int16): boolean;
var
    e: int8;
    found: boolean;
begin
    if seen[u] then
        found := false
    else if go[u] = '.' then
        found := true
    else if go[u] = 'B' then
        found := false
    else begin
        found := false;
        seen[u] := true;
        e := 1;
        while (e <= 4) and not found do begin
            found := not dfs(adj[u, e]);
            inc(e);
        end;
    end;
    dfs := not found;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(r, c);

        for i := 1 to r do begin
            go[ij(i, 0)] := 'B';
            go[ij(i, c+1)] := 'B';
        end;

        for j := 1 to c do begin
            go[ij(0, j)] := 'B';
            go[ij(r+1, j)] := 'B';
        end;

        for i := 1 to r do begin
            for j := 1 to c do begin
                u := ij(i, j);
                read(go[u]);
                adj[u, 1] := ij(i, j+1);
                adj[u, 2] := ij(i-1, j);
                adj[u, 3] := ij(i, j-1);
                adj[u, 4] := ij(i+1, j);
            end;
            readln;
        end;

        ans := false;

        for i := 1 to r do if not ans then begin
            for j := 1 to c do if not ans then begin
                u := ij(i, j);
                if go[u] = '.' then begin

                    e := 1;
                    while (e <= 4) and not ans do begin
                        if go[adj[u, e]] = 'W' then begin
                            for v := 0 to maxrc do seen[v] := false;
                            seen[u] := true;
                            v := adj[u, e];
                            ans := ans or dfs(v);
                        end;
                        inc(e);
                    end;

                end;
            end;
        end;

        write('Case #', tci, ': ');
        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
