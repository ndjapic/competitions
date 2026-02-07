program B_Traveling_Takahashi_Problem;
{$mode delphi}
uses
    math;
const
    nn = 200 * 1000 + 1;
var
    n, i: int32;
    ans: extended;
    x, y: array [0 .. nn] of int32;

function dist(dx, dy: int64): extended;
begin
    result := sqr(dx) + sqr(dy);
    result := sqrt(result);
end;

begin
    readln(n);
    for i := 1 to n do readln(x[i], y[i]);
    x[0] := 0;
    y[0] := 0;
    x[n+1] := 0;
    y[n+1] := 0;

    ans := 0.0;
    for i := 1 to n+1 do
        ans := ans + dist(x[i-1] - x[i], y[i-1] - y[i]);

    writeln(ans:25:7);
end.
