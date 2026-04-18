program B_Foreign_Exchange;
const
    maxn = 200 * 1000;
var
    n, i, s, t: int32;
    a: array [1 .. maxn] of int64;

begin
    readln(n);
    for i := 1 to n do read(a[i]); readln;

    for i := 1 to n-1 do begin
        readln(s, t);
        inc(a[i+1], a[i] div s * t);
    end;

    writeln(a[n]);
end.
