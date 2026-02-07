program F_Small_Operations;
uses
    math;
const
    nn = 1000 * 1000;
    tt = 10 * 1000;
    inf = 40;
var
    ntc, i, k, g, u, v: int32;
    dist, last: array [1 .. nn] of int32;
    x, y, prev, ans: array [1 .. tt] of int32;

function gcd(x, y: int32): int32;
begin
    if y = 0 then
        gcd := x
    else
        gcd := gcd(y, x mod y);
end;

begin
    for k := 1 to nn do last[k] := 0;

    readln(ntc);
    for i := 1 to ntc do begin
        readln(x[i], y[i], k);
        prev[i] := last[k];
        last[k] := i;
    end;

    for v := 2 to nn do dist[v] := inf;
    dist[1] := 0;

    for k := 1 to nn do begin

        for u := 1 to nn div k do begin
            v := u * k;
            dist[v] := min(dist[v], dist[u] + 1);
        end;

        i := last[k];
        while i > 0 do begin
            g := gcd(x[i], y[i]);
            ans[i] := dist[x[i] div g] + dist[y[i] div g];
            if ans[i] >= inf then ans[i] := -1;
            i := prev[i];
        end;

    end;

    for i := 1 to ntc do writeln(ans[i]);

end.
