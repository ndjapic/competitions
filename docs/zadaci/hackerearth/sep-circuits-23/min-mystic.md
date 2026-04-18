# Задатак: min-mystic.pas

```pascal
{$IOCHECKS OFF}
program min_mystic;
uses
    math;
const
    maxn = 1000 * 1000;
var
    ntc, n, u, v, i: int32;
    ans: int64;
    adj, par: array [1 .. maxn] of int32;
    sib, tar, w: array [-maxn .. maxn] of int32;
    c, tw, mys: array [1 .. maxn] of array [0 .. 2] of int64;

procedure addarrow(u, v, i: int32);
begin
    sib[i] := adj[u];
    adj[u] := i;
    tar[i] := v;
end;

procedure dfs(u: int32);
var
    v, i: int32;
begin
    c[u][0] := 1;
    c[u][1] := 0;
    c[u][2] := 0;
    tw[u][1] := 0;
    tw[u][2] := 0;
    mys[u][0] := 0;
    mys[u][1] := 0;
    mys[u][2] := 0;

    i := adj[u];
    while i <> 0 do begin
        v := tar[u];
        if par[u] <> v then begin

            par[v] := u;
            dfs(v);

            inc(c[u][1], c[v][0]);
            inc(c[u][2], c[v][1]);
            inc(c[u][0], c[v][2]);

            inc(tw[u][1], w[i] * c[v][0]);
            inc(tw[u][2], w[i] + tw[v][1]);

            inc(mys[u][1], mys[v][0]);
            inc(mys[u][2], mys[v][1]);
            inc(mys[u][0], mys[v][2]);

        end;
        i := sib[i];
    end;

    inc(mys[u][0], tw[u][1]);
    inc(mys[u][0], (tw[u][1] + tw[u][2]) * 2);

end;

begin
    readln(ntc);
    repeat

        readln(n);

        for v := 1 to n do adj[v] := 0;

        for i := 1 to n-1 do begin
            readln(u, v, w[i]);
            w[-i] := w[i];
            addarrow(u, v, i);
            addarrow(v, u, -i);
        end;

        ans := high(int64);
        for u := 1 to n do begin
            par[u] := 0;
            dfs(u);
            ans := min(ans, mys[u][0]);
        end;
        writeln(ans);

        dec(ntc);
    until ntc = 0;
end.

```
