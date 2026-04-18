# Задатак: C_Disappearing_Permutation.pas

```pascal
program C_Disappearing_Permutation;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, d, ans: int32;
    dsu, size: array [1 .. nn] of int32;

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
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            dsu[i] := i;
            size[i] := 1;
        end;

        for i := 1 to n do begin
            read(d);
            union1(i, d);
        end;
        readln;

        ans := 0;
        for i := 1 to n do begin
            read(d);
            d := find(d);
            inc(ans, size[d]);
            size[d] := 0;
            write(ans, ' ');
        end;
        readln;
        writeln;

    end;
end.

```
