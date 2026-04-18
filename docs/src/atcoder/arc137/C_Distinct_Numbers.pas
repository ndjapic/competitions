program C_Distinct_Numbers;
const
    nn = 300 * 1000;
var
    n, i: int32;
    a: array [0 .. nn] of int32;

begin
    readln(n);

    a[0] := -1;
    for i := 1 to n do read(a[i]); readln;

    if odd(a[n] - a[n-1] - 1) or odd(a[n] - n + 1) then
        writeln('Alice')
    else
        writeln('Bob');
end.
