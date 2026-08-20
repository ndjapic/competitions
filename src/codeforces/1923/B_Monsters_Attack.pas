program B_Monsters_Attack;
uses
    math;
const
    maxn = 300 * 1000;
var
    ntc, tci: int16;
    n, i, j, k: int32;
    fired: int64;
    ans: boolean;
    a, x: array [1 .. maxn] of int32;
    health: array [-maxn .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(x[i]); readln;

        for j := -n to n do health[j] := 0;

        for i := 1 to n do
            health[x[i]] := a[i];

        fired := 0;
        j := 0;
        ans := true;

        while (j < n) and ans do begin
            inc(j);
            inc(fired, k);
            dec(fired, health[j] + health[-j]);
            ans := fired >= 0;
        end;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;

end.
