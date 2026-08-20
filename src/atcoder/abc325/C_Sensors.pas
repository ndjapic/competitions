program C_Sensors;
const
    maxh = 1000;
    maxhw = 1000 * 1000;
var
    h, w, i, j: int16;
    hw, v, ans: int32;
    s: array [1 .. maxhw] of char;
    dsu, sz: array [1 .. maxhw] of int32;

function find(v: int32): int32;
begin
    if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
    find := dsu[v];
end;

procedure union1(u, v: int32);
begin
    dsu[v] := u;
    inc(sz[u], sz[v]);
end;

procedure union2(u, v: int32);
begin
    if (s[u] = '#') and (s[v] = '#') then begin
        u := find(u);
        v := find(v);
        if u <> v then begin
            if sz[u] > sz[v] then
                union1(u, v)
            else
                union1(v, u);
        end;
    end;
end;

function ij(i, j: int16): int32;
begin
    ij := (i-1)*w + j;
end;

begin
    readln(h, w);
    hw := h*w;

    for i := 1 to h do begin
        for j := 1 to w do read(s[ij(i, j)]);
        readln;
    end;

    for v := 1 to hw do begin
        dsu[v] := v;
        sz[v] := 1;
    end;

    for i := 1 to h do
        for j := 1 to w-1 do
            union2(ij(i, j), ij(i, j+1));

    for i := 1 to h-1 do
        for j := 1 to w do
            union2(ij(i, j), ij(i+1, j));

    for i := 1 to h-1 do
        for j := 1 to w-1 do begin
            union2(ij(i, j), ij(i+1, j+1));
            union2(ij(i, j+1), ij(i+1, j));
        end;

    {for i := 1 to h do begin
        for j := 1 to w do write(' ', dsu[ij(i, j)]);
        writeln;
    end;}

    ans := 0;
    for v := 1 to hw do
        if (s[v] = '#') and (find(v) = v) then inc(ans);

    writeln(ans);
end.
