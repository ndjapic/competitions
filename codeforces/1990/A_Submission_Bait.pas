program A_Submission_Bait;
const
    nn = 50;
var
    ntc, tci: int16;
    n, i, x: int8;
    c: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for x := 1 to n do c[x] := 0;

        for i := 1 to n do begin
            read(x);
            inc(c[x]);
        end;
        readln;

        x := 1;
        while (x <= n) and not odd(c[x]) do inc(x);

        if x <= n then
            writeln('YES')
        else
            writeln('NO');

    end;
end.
