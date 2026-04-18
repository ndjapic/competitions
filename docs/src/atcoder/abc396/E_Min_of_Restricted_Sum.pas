program E_Min_of_Restricted_Sum;
const
    nn = 200 * 1000;
    mm = 100 * 1000;

var
    n, m, u, v: int32;
    e: int8;
    ans: boolean;
    a, leader: array [1 .. nn] of int32;
    c: array [1 .. nn, 0 .. 29, 0 .. 1] of int32;
    z: array [1 .. mm] of int32;
    adj: array [1 .. nn] of int32;
    sib, tar: array [-mm .. mm] of int32;

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
        a[v] := -1;
    end;

    for i := 1 to m do begin
        readln(u, v, z[i]);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;

procedure dfs(u: int32);
var
    i, v, x: int32;
    e: int8;
begin
    if ans then begin
        for e := 0 to 29 do begin
            c[u, e, 0] := 0;
            c[u, e, 1] := 0;
            inc(c[u, e, (a[u] shr e) and 1]);
        end;

        i := adj[u];
        while i <> 0 do begin
            x := a[u] xor z[abs(i)];
            v := tar[i];

            if a[v] = -1 then begin

                a[v] := x;
                leader[v] := leader[u];
                dfs(v);

                for e := 0 to 29 do begin
                    inc(c[u, e, 0], c[v, e, 0]);
                    inc(c[u, e, 1], c[v, e, 1]);
                end;

            end else
                ans := a[v] = x;

            i := sib[i];
        end;
    end;
end;

begin
    readln(n, m);
    readedges(n, m);
    ans := true;

    for u := 1 to n do
        if ans and (a[u] = -1) then begin

            a[u] := 0;
            leader[u] := u;
            dfs(u);

            if ans then
                for e := 0 to 29 do
                    if c[u, e, 1] > c[u, e, 0] then
                        inc(a[u], 1 shl e);

        end;

    if ans then begin
        for v := 1 to n do begin
            u := leader[v];
            if u <> v then a[v] := a[v] xor a[u];
            write(a[v]);
            if v < n then write(' ');
        end;
        writeln;
    end else
        writeln(-1);
end.
