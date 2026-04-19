# Problem: D_Election_Quick_Report.pas

```pascal
program D_Election_Quick_Report;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, m, i, candidate, winner: int32;
    votes: array [1 .. maxn] of int32;

begin
    readln(n, m);

    for candidate := 1 to n do votes[candidate] := 0;
    winner := 1;

    for i := 1 to m do begin
        read(candidate);
        inc(votes[candidate]);
        if (votes[candidate] > votes[winner]) or (votes[candidate] = votes[winner]) and (candidate < winner) then
            winner := candidate;
        writeln(winner);
    end;
    readln;
end.

```
