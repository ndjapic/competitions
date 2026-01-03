program A_01_Matrix_Again;
const
    maxn = 100 * 1000;
var
    n, m, i, a, b, d, x: int32;
    seen: array [0 .. maxn] of boolean;

begin
    readln(n, m);

    for d := 0 to n-1 do seen[d] := false;

    x := 0;
    for i := 1 to m do begin
        readln(a, b);
        d := a - b;
        if d < 0 then inc(d, n);
        if seen[d] then begin
            while seen[x] do inc(x);
            d := x;
        end;
        seen[d] := true;
    end;

    writeln(n*m);
    for d := 0 to n-1 do
        if seen[d] then begin
            for i := d+1 to n do writeln(i, ' ', i-d);
            for i := 1 to d do writeln(i, ' ', n+i-d);
        end;
end.
