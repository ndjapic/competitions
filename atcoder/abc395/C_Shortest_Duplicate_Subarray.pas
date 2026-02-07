program C_Shortest_Duplicate_Subarray;
uses
    math;
const
    nn = 200 * 1000;
    xx = 1000 * 1000;
var
    n, i, ai, mn: int32;
    pos: array [1 .. xx] of int32;

begin
    readln(n);

    for ai := 1 to xx do pos[ai] := 0;

    mn := n+1;
    for i := 1 to n do begin
        read(ai);
        if pos[ai] > 0 then
            mn := min(mn, i - pos[ai] + 1);
        pos[ai] := i;
    end;
    readln;

    if mn = n+1 then mn := -1;
    writeln(mn);
end.
