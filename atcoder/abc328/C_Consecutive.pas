program C_Consecutive;
const
    maxn = 300 * 1000;
var
    n, q, i, l, r: int32;
    s: array [1 .. maxn] of char;
    c: array [1 .. maxn] of int32;

begin
    readln(n, q);

    c[1] := 0;
    read(s[1]);

    for i := 2 to n do begin
        read(s[i]);
        c[i] := c[i-1];
        if s[i] = s[i-1] then inc(c[i]);
    end;
    readln;

    for i := 1 to q do begin
        readln(l, r);
        writeln(c[r] - c[l]);
    end;
end.
