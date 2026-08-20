program B_Begginer_s_Zelda;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, u, v, leaves: int32;
    deg: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);

        for v := 1 to n do deg[v] := 0;

        for i := 1 to n-1 do begin
            readln(u, v);
            inc(deg[u]);
            inc(deg[v]);
        end;

        leaves := 0;
        for v := 1 to n do
            if deg[v] = 1 then inc(leaves);

        writeln((leaves + 1) div 2);
    end;
end.
