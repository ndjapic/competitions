program B_Not_All;
const
    nn = 100;
var
    n, m, i, x, t: int8;
    a, c: array [1 .. nn] of int8;

begin
    readln(n, m);

    for x := 1 to m do c[x] := 0;

    for i := 1 to n do begin
        read(a[i]);
        inc(c[a[i]]);
    end;
    readln;

    t := 0;
    for x := 1 to m do
        if c[x] > 0 then inc(t);

    i := n;
    while t = m do begin
        dec(c[a[i]]);
        if c[a[i]] = 0 then dec(t);
        dec(i);
    end;

    writeln(n-i);
end.
