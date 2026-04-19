# Problem: D_New_Friends.pas

```pascal
program D_New_Friends;
const
    maxn = 200 * 1000;
var
    n, m, i, a, b: int32;
    ans: int64;
    dsu, size: array [1 .. maxn] of int32;

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union2(u, v: int32);
begin
    dsu[v] := u;
    inc(size[u], size[v]);
end;

procedure union1(u, v: int32);
begin
    u := find(u);
    v := find(v);
    if u = v then
    else if size[u] > size[v] then
        union2(u, v)
    else
        union2(v, u);
end;

begin
    readln(n, m);

    for a := 1 to n do begin
        dsu[a] := a;
        size[a] := 1;
    end;

    for i := 1 to m do begin
        readln(a, b);
        union1(a, b);
    end;

    ans := -m;
    for a := 1 to n do
        if dsu[a] = a then
            inc(ans, int64(size[a] - 1) * size[a] div 2);

    writeln(ans);
end.

```
