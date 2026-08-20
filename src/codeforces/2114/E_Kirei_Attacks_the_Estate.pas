program E_Kirei_Attacks_the_Estate;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, v: int32;
    adj, par: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;
    a, s, threat: array [1 .. nn] of int64;

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
        read(a[v]);
        adj[v] := 0;
    end;
    readln;

    for i := 1 to m do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;

procedure dfs(u: int32);
var
    i, v: int32;
begin
    i := adj[u];
    while i <> 0 do begin
        v := tar[i];
        if par[u] <> v then begin
            par[v] := u;
            threat[v] := a[v] - s[u];
            s[v] := min(0, a[v] - threat[u]);
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

        par[1] := 1;
        s[1] := 0;
        threat[1] := a[1];
        dfs(1);

        for v := 1 to n-1 do write(threat[v], ' ');
        writeln(threat[n]);

    end;
end.
