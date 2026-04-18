program D_Counting_Ls;
uses
    math;
const
    maxn = 2000;
var
    n, i, j: int16;
    ans: int64;
    s: array [1 .. maxn, 1 .. maxn] of char;
    r, c: array [1 .. maxn] of int16;

begin
    readln(n);

    for j := 1 to n do c[j] := 0;

    for i := 1 to n do begin
        r[i] := 0;
        for j := 1 to n do begin
            read(s[i, j]);
            if s[i, j] = 'o' then begin
                inc(r[i]);
                inc(c[j]);
            end;
        end;
        readln;
    end;

    ans := 0;
    for i := 1 to n do
        for j := 1 to n do
            if s[i, j] = 'o' then
                inc(ans, int64(r[i] - 1) * (c[j] - 1));

    writeln(ans);
end.
