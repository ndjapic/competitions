program A_12435;
const
    n = 5;
var
    i: int8;
    ans: boolean;
    a: array [1 .. n] of int8;

begin
    for i := 1 to n do read(a[i]);
    readln;

    i := 2;
    while (i <= n) and (a[i-1] < a[i]) do inc(i);

    ans := i <= n;
    if ans then begin
        a[i] := a[i-1];
        while (i <= n) and (a[i-1] <= a[i]) do inc(i);
        ans := i > n;
    end;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.
