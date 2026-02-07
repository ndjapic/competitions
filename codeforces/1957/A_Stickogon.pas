program A_Stickogon;
const
    maxn = 100;
var
    ntc, tci: int8;
    n, i, ai, ans: int8;
    c: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for ai := 1 to maxn do c[ai] := 0;

        for i := 1 to n do begin
            read(ai);
            inc(c[ai]);
        end;
        readln;

        ans := 0;
        for ai := 1 to maxn do
            inc(ans, c[ai] div 3);

        writeln(ans);

    end;
end.
