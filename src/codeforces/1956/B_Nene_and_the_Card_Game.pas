program B_Nene_and_the_Card_Game;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, x, i, ans: int32;
    c: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for x := 1 to n do c[x] := 0;
        ans := 0;

        for i := 1 to n do begin
            read(x);
            inc(c[x]);
            if c[x] = 2 then inc(ans);
        end;
        readln;

        writeln(ans);

    end;
end.
