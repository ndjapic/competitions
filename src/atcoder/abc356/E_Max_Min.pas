program E_Max_Min;
uses
    math;
const
    xx = 1000 * 1000;
var
    n, i, x, y, d: int32;
    ans: int64;
    c: array [0 .. xx] of int32;

begin
    readln(n);

    for x := 0 to xx do c[x] := 0;

    for i := 1 to n do begin
        read(x);
        inc(c[x]);
    end;
    readln;

    ans := 0;
    for x := 1 to xx do begin
        dec(ans, int64(c[x]+1) * c[x] div 2);
        inc(c[x], c[x-1]);
    end;

    for x := 1 to xx do begin
        for d := 1 to xx div x do begin
            y := x * d - 1;
            inc(ans, int64(c[x] - c[x-1]) * (c[min(xx, y+x)] - c[y]) * d);
        end;
    end;

    writeln(ans);
end.
