program D_Tile_Pattern;
const
    maxn = 1000;
var
    n, q, i, j, a, b, c, d: int32;
    p: array [0 .. maxn] of array [0 .. maxn] of char;
    s: array [0 .. maxn] of array [0 .. maxn] of int32;

function f(c, d: int32): int64;
begin
    if (c < n) and (d < n) then
        f := s[c+1, d+1]
    else if c < n then
        f := f(c, d mod n) + f(c, n-1) * (d div n)
    else
        f := f(c mod n, d) + f(n-1, d) * (c div n);
end;

begin
    readln(n, q);

    for j := 0 to n do s[0, j] := 0;

    for i := 0 to n-1 do begin
        s[i, 0] := 0;
        for j := 0 to n-1 do begin

            read(p[i][j]);
            s[i+1][j+1] := s[i][j+1] + s[i+1][j] - s[i][j];
            if p[i][j] = 'B' then inc(s[i+1][j+1]);

        end;
        readln;
    end;

    for i := 1 to q do begin
        readln(a, b, c, d);
        writeln(f(c, d) - f(a-1, d) - f(c, b-1) + f(a-1, b-1));
    end;
end.
