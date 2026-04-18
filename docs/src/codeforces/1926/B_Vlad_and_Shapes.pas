program B_Vlad_and_Shapes;
{$H+}
uses
    math;
const
    maxn = 10;
var
    ntc, tci, n, i, j, mn, mx: int8;
    s: array [1 .. maxn] of string;
    c: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        mn := n;
        mx := 0;

        for i := 1 to n do begin
            readln(s[i]);
            c[i] := 0;
            for j := 1 to n do inc(c[i], ord(s[i][j]) - ord('0'));
            mn := min(mn, c[i]);
            mx := max(mx, c[i]);
        end;

        i := 1;
        while (i <= n) and ((c[i] = mn) or (c[i] = mx)) do inc(i);

        if i <= n then
            writeln('TRIANGLE')
        else
            writeln('SQUARE');

    end;
end.
