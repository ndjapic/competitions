program B_Spot_the_Difference;
{$H+}
const
    sz = 100;
var
    n, i, j: int8;
    a, b: array [1 .. sz] of string;

begin
    readln(n);
    for i := 1 to n do readln(a[i]);
    for i := 1 to n do readln(b[i]);

    i := 1;
    j := 1;
    while a[i][j] = b[i][j] do begin
        inc(j);
        if j > n then begin
            j := 1;
            inc(i);
        end;
    end;

    writeln(i, ' ', j);
end.
