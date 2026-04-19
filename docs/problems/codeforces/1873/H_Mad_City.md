# Problem: H_Mad_City.pas

```pascal
program H_Mad_City;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, a, b, l, r, d: int32;
    adj, par, dist, bfs: array [1 .. nn] of int32;
    sib, tar: array [-nn .. nn] of int32;

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
        par[v] := 0;
        dist[v] := 0;
    end;

    for i := 1 to m do begin
        readln(u, v);
        addarrow(u, v, i);
        addarrow(v, u, -i);
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, a, b);
        readedges(n, n);

        bfs[1] := b;
        l := 1;
        r := 1;
        dist[b] := 0;

        while l <= r do begin

            u := bfs[l];
            inc(l);
            d := dist[u] + 1;

            i := adj[u];
            while (i <> 0) and loop do begin
                v := tar[i];
                if par[u] <> v then begin

                    if par[v] = 0 then begin
                        par[v] := u;
                        dist[v] := d;
                        inc(r);
                        bfs[r] := v;
                    end else begin
                        u0 := u;
                        v0 := v;
                    end;

                end;
                i := sib[i];
            end;

        end;

    end;
end.

```
