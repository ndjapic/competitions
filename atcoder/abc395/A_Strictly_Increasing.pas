program A_Strictly_Increasing;
const
    nn = 100;
var
    n, i: int8;
    a: array [1 .. nn] of int16;

begin
    readln(n);

    for i := 1 to n do read(a[i]); readln;

    i := 1;
    while (i < n) and (a[i] < a[i+1]) do inc(i);

    if i < n then
        writeln('No')
    else
        writeln('Yes');
end.
