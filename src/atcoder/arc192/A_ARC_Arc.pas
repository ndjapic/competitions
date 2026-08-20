program A_ARC_Arc;
const
    nn = 200 * 1000 + 10;

var
    n, i: int32;
    a: array [1 .. nn] of int8;

begin
    readln(n);

    for i := 1 to n do begin
        read(a[i]);
        a[i] := 1-a[i];
    end;
    for i := n+1 to n+10 do a[i] := a[i-n];
    readln;

    i := 1;
    while (i <= n) and (
        (a[i] = 1) or
        (a[i+1] = 1) or
        (a[i+2] = 1) or
        (a[i+3] = 1) or
        (a[i+4] = 1)
    ) do inc(i);

    if i > n then
        writeln('Yes')
    else begin

        writeln('No');
    end;
end.
