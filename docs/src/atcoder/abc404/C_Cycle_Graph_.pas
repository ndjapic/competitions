program C_Cycle_Graph_;
{$MODE DELPHI}
uses
    math;
const
    nn = 200 * 1000;
var
    n, m, i, u, v, l, r: int32;
    ans: boolean;
    adj, bfs: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;
    seen: array [1 .. nn] of boolean;

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
        seen[v] := false;
    end;

    for i := 1 to m do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;

begin
    readln(n, m);
    readedges(n, m);

    ans := n = m;

    {writeln('n=m is ', n=m);}

    if ans then begin
        u := 1;
        while (u <= n) and ans do begin
            i := adj[u];
            ans := i <> 0;
            if ans then begin
                i := sib[i];
                ans := (i <> 0) and (sib[i] = 0);
            end;
            inc(u);
        end;
    end;

    {writeln('deg = 2 is ', ans);}

    if ans then begin
        i := adj[1];
        v := tar[i];

        seen[1] := true;
        seen[v] := true;
        bfs[1] := 1;
        bfs[2] := v;

        l := 2;
        r := 2;

        while l <= r do begin
            u := bfs[l];
            inc(l);
            i := adj[u];
            while i <> 0 do begin
                v := tar[i];
                if not seen[v] then begin
                    seen[v] := true;
                    inc(r);
                    bfs[r] := v;
                end;
                i := sib[i];
            end;
        end;

        ans := r = n;
    end;

    {writeln('r=n is ', ans);}

    if ans then begin
        ans := false;
        u := bfs[r];
        i := adj[u];

        while (i <> 0) and not ans do begin
            v := tar[i];
            ans := v = 1;
            i := sib[i];
        end;
    end;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.
