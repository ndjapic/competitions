# Problem: covid_cure.pas

```pascal
{$IOCHECKS OFF}
program covid_cure;
const
    maxn = 10 * 1000;
    maxm = 200 * 1000;
type
    tarr = array [1 .. maxn] of int64;
var
    n, m, k, u, v, e: int32;
    adj, par, a: array [1 .. maxn] of int32;
    b, c, d, zb, zd: tarr;
    sib, tar: array [-maxm .. maxm] of int32;
    seen: array [1 .. maxn] of boolean;

procedure addarrow(u, v, e: int32);
begin
    sib[e] := adj[u];
    adj[u] := e;
    tar[e] := v;
end;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (d[il] < d[ir]) then begin
                zb[i] := b[il];
                zd[i] := d[il];
                inc(il);
            end else begin
                zb[i] := b[ir];
                zd[i] := d[ir];
                inc(ir);
            end;

        for i := l to r do begin
            b[i] := zb[i];
            d[i] := zd[i];
        end;

    end;
end;

begin
    readln(n, m);

    for u := 1 to n do begin
        adj[u] := 0;
        seen[u] := false;
    end;

    for e := 1 to m do begin
        readln(u, v);
        addarrow(u, v, e);
        addarrow(v, u, -e);
    end;

    for u := 1 to n do read(b[u]); readln;

    for u := 1 to n do begin
        read(c[u]);
        dec(b[u], c[u]);
    end;
    readln;

    for u := 1 to n do begin
        d[u] := b[u] * 3;

        e := adj[u];
        while e <> 0 do begin
            v := tar[e];
            dec(d[u], b[v]);
            e := sib[e];
        end;
    end;

    msort(1, n);

    k := 0;
    for u := n downto 1 do
        if not seen[u] then begin

            {seen[u] := true;}
            inc(k);
            a[k] := u;

            e := adj[u];
            while e <> 0 do begin
                v := tar[e];
                seen[v] := true;
                e := sib[e];
            end;

        end;

    writeln(k);
    while k > 1 do begin
        write(a[k], ' ');
        dec(k);
    end;
    writeln(a[k]);
end.

```
