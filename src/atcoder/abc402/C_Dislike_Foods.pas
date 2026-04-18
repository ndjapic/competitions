program C_Dislike_Foods;
const
    nn = 300 * 1000;
var
    n, m, i, j, x, d, c: int32;
    adj: array [1 .. nn] of array of int32;
    deg, k: array [1 .. nn] of int32;

begin
    readln(n, m);

    for j := 1 to n do begin
        deg[j] := 0;
        setlength(adj[j], 1);
    end;

    for i := 1 to m do begin
        read(k[i]);
        for x := 0 to k[i]-1 do begin
            read(j);
            if length(adj[j]) = deg[j] then setlength(adj[j], 2*deg[j]);
            adj[j][deg[j]] := i;
            inc(deg[j]);
        end;
        readln;
    end;

    c := 0;
    for d := 1 to n do begin
        read(j);
        for x := 0 to deg[j]-1 do begin
            i := adj[j][x];
            dec(k[i]);
            if k[i] = 0 then inc(c);
        end;
        writeln(c);
    end;
    readln;
end.
