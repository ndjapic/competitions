program C_Brr_Brrr_Patapim;
const
    nn = 800;
var
    ntc, tci: int16;
    n, i, j, x: int16;
    p: array [1 .. 2*nn] of int16;
    seen: array [1 .. 2*nn] of boolean;
    g: array [1 .. nn, 1 .. nn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for x := 1 to 2*n do seen[x] := false;

        for i := 1 to n do begin
            for j := 1 to n do begin
                read(g[i, j]);
                p[i+j] := g[i, j];
                seen[p[i+j]] := true;
            end;
            readln;
        end;

        p[1] := 1;
        while seen[p[1]] do inc(p[1]);

        for i := 1 to 2*n-1 do write(p[i], ' ');
        writeln(p[2*n]);

    end;
end.
