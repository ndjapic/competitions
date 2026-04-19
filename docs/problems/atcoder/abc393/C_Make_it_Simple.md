# Problem: C_Make_it_Simple.pas

```pascal
program C_Make_it_Simple;
const
    nn = 200 * 1000;
    mm = 500 * 1000;
type
    tarr = array of int32;
var
    n, m, i, j, u, v, w, ans: int32;
    adj: array [1 .. nn] of tarr;
    deg: array [1 .. nn] of int32;
    merge: array [0 .. mm] of int32;

procedure msort(var a: tarr; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(a, l, m);
        msort(a, m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                a[j] <= a[k]
            ) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := merge[i];

    end;
end;

begin
    readln(n, m);

    for v := 1 to n do begin
        setlength(adj[v], 1);
        deg[v] := 0;
    end;

    ans := 0;
    for i := 1 to m do begin
        readln(u, v);
        if u = v then
            inc(ans)
        else begin
            if u > v then begin
                w := u;
                u := v;
                v := w;
            end;
            if deg[u] = length(adj[u]) then setlength(adj[u], deg[u] * 2);
            adj[u][deg[u]] := v;
            inc(deg[u]);
        end;
    end;

    for v := 1 to n do begin
        msort(adj[v], 0, deg[v]);
        for j := 1 to deg[v] - 1 do
            if adj[v][j-1] = adj[v][j] then inc(ans);
    end;

    writeln(ans);
end.

```
