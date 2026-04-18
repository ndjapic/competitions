program C_Illuminate_Buildings;
uses
    math;
const
    nn = 1000;
var
    n, i, d, c, ans: int16;
    h: array [1 .. nn] of int16;

begin
    readln(n);

    for i := 1 to n do read(h[i]); readln;

    ans := 1;
    for i := 1 to n-1 do begin
        d := 1;
        while i + ans * d <= n do begin
            c := 1;
            while (i+c*d <= n) and (h[i+c*d] = h[i]) do inc(c);
            ans := max(ans, c);
            inc(d);
        end;
    end;

    writeln(ans);
end.
