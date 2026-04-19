# Problem: dsu.pas

```pascal
program A_Disjoint_Set_Union;
const
    nn = 200 * 1000;
var
    n, q, i, u, v: int32;
    tp: int8;
    dsu, size: array [0 .. nn] of int32;

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
    readln(n, q);

    for v := 0 to n-1 do begin
        dsu[v] := v;
        size[v] := 1;
    end;

    for i := 1 to q do begin
        readln(tp, u, v);
        case tp of

            0: union1(u, v);

            1: if find(u) = find(v) then
                writeln('1')
            else
                writeln('0');

        end;
    end;
end.

```
