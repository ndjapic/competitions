program abc305_f;
var
    n, m, u: int16;
    found: boolean;
    seen: array [1 .. 100] of boolean;

procedure dfs(u: int16);
var
    k, i, k2, i2, v2: int16;
    v: array [1 .. 100] of int16;
begin
    if not found then begin

        seen[u] := true;
        read(k);
        for i := 1 to k do read(v[i]);
        readln;

        for i := k downto 1 do
            if not seen[v[i]] and not found then begin

                writeln(v[i]);
                flush(output);
                found := found or (v[i] = n);
                dfs(v[i]);

                if not found then begin
                    writeln(u);
                    flush(output);
                    read(k2);
                    for i2 := 1 to k2 do read(v2);
                    readln;
                end;

            end;

    end;
end;

begin
    readln(n, m);
    found := false;
    for u := 1 to n do seen[u] := false;
    dfs(1);
end.

