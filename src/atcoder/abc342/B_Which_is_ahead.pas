program B_Which_is_ahead;
const
    maxn = 100;
var
    n, q, i, a, b: int8;
    p, pinv: array [1 .. maxn] of int8;

begin
    readln(n);

    for i := 1 to n do begin
        read(p[i]);
        pinv[p[i]] := i;
    end;
    readln;

    readln(q);
    for i := 1 to q do begin
        readln(a, b);
        if pinv[a] < pinv[b] then
            writeln(a)
        else
            writeln(b);
    end;
end.
