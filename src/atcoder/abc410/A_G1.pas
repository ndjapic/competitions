program A_G1;
var
    n, i, k, c: int8;
    a: array [1 .. 100] of int8;

begin
    readln(n);
    for i := 1 to n do read(a[i]); readln;

    readln(k);
    c := 0;
    for i := 1 to n do
        if k <= a[i] then inc(c);

    writeln(c);
end.
