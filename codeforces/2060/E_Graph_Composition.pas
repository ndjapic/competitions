program E_Graph_Composition;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, j, ans: int32;
    k: int8;
    m: array [1 .. 2] of int32;
    u, v, dsu, size: array [1 .. 2] of array [1 .. nn] of int32;

function find(k: int8; v: int32): int32;
begin
    if dsu[k][dsu[k][v]] <> dsu[k][v] then dsu[k][v] := find(k, dsu[k][v]);
    find := dsu[k][v];
end;

procedure union2(k: int8; u, v: int32);
begin
    dsu[k][v] := u;
    inc(size[k][u], size[k][v]);
end;

procedure union1(k: int8; u, v: int32);
begin
    u := find(k, u);
    v := find(k, v);
    if u = v then
    else if size[k][u] > size[k][v] then
        union2(k, u, v)
    else
        union2(k, v, u);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m[1], m[2]);

        for k := 1 to 2 do begin
            for i := 1 to n do begin
                dsu[k][i] := i;
                size[k][i] := 1;
            end;
            for j := 1 to m[k] do readln(u[k][j], v[k][j]);
        end;

        for j := 1 to m[2] do union1(2, u[2][j], v[2][j]);

        ans := 0;
        for j := 1 to m[1] do
            if find(2, u[1][j]) = find(2, v[1][j]) then
                union1(1, u[1][j], v[1][j])
            else
                inc(ans);

        for j := 1 to m[2] do
            if find(1, u[2][j]) <> find(1, v[2][j]) then begin
                union1(1, u[2][j], v[2][j]);
                inc(ans);
            end;

        writeln(ans);

    end;
end.
